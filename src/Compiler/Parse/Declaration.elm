module Compiler.Parse.Declaration exposing
    ( Decl(..)
    , declaration
    , infix_
    )

import Compiler.AST.Source as Src
import Compiler.AST.Utils.Binop as Binop
import Compiler.Data.Name exposing (Name)
import Compiler.Parse.Expression as Expr
import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.NewPrimitives as P
import Compiler.Parse.Number as Number
import Compiler.Parse.Pattern as Pattern
import Compiler.Parse.Space as Space
import Compiler.Parse.Symbol as Symbol
import Compiler.Parse.SyntaxVersion exposing (SyntaxVersion)
import Compiler.Parse.Type as Type
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Parser exposing (..)
import Parser.Advanced as Advanced



-- DECLARATION


type Decl
    = Value (Maybe Src.Comment) (A.Located Src.Value)
    | Union (Maybe Src.Comment) (A.Located Src.Union)
    | Alias (Maybe Src.Comment) (A.Located Src.Alias)
    | Port (Maybe Src.Comment) Src.Port


declaration : SyntaxVersion -> Space.Parser E.Decl Decl
declaration syntaxVersion =
    chompDocComment
        |> andThen
            (\maybeDocs ->
                P.getPosition
                    |> andThen
                        (\start ->
                            oneOf
                                [ typeDecl maybeDocs start
                                , portDecl maybeDocs
                                , valueDecl syntaxVersion maybeDocs start
                                ]
                        )
            )



-- DOC COMMENT


chompDocComment : P.Parser E.Decl (Maybe Src.Comment)
chompDocComment =
    oneOf
        [ Space.docComment E.DeclStart E.DeclSpace
            |> andThen
                (\docComment ->
                    Space.chomp E.DeclSpace
                        |> andThen (\_ -> Space.checkFreshLine E.DeclFreshLineAfterDocComment)
                        |> map (\_ -> Just docComment)
                )
        ]
        |> oneOfWithFallback Nothing



-- DEFINITION and ANNOTATION


valueDecl : SyntaxVersion -> Maybe Src.Comment -> A.Position -> Space.Parser E.Decl Decl
valueDecl syntaxVersion maybeDocs start =
    Var.lower E.DeclStart
        |> andThen
            (\name ->
                P.getPosition
                    |> andThen
                        (\end ->
                            oneOf
                                [ Advanced.symbol ":" E.DeclDefEquals
                                    |> andThen (\_ -> Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentType)
                                    |> andThen (\_ -> Type.expression)
                                    |> andThen
                                        (\( tipe, _ ) ->
                                            Space.checkFreshLine E.DeclDefNameRepeat
                                                |> andThen (\_ -> chompMatchingName name)
                                                |> andThen
                                                    (\defName ->
                                                        Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
                                                            |> andThen (\_ -> chompDefArgsAndBody syntaxVersion maybeDocs start defName (Just tipe) [])
                                                    )
                                        )
                                , chompDefArgsAndBody syntaxVersion maybeDocs start (A.at start end name) Nothing []
                                ]
                        )
            )


chompDefArgsAndBody : SyntaxVersion -> Maybe Src.Comment -> A.Position -> A.Located Name -> Maybe Src.Type -> List Src.Pattern -> Space.Parser E.DeclDef Decl
chompDefArgsAndBody syntaxVersion maybeDocs start name tipe revArgs =
    oneOf
        [ Pattern.term syntaxVersion
            |> andThen
                (\arg ->
                    Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
                        |> andThen (\_ -> chompDefArgsAndBody syntaxVersion maybeDocs start name tipe (arg :: revArgs))
                )
        , Advanced.symbol "=" E.DeclDefEquals
            |> andThen (\_ -> Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentBody)
            |> andThen (\_ -> Expr.expression syntaxVersion)
            |> map
                (\( body, end ) ->
                    let
                        value : Src.Value
                        value =
                            Src.Value name (List.reverse revArgs) body tipe

                        avalue : A.Located Src.Value
                        avalue =
                            A.at start end value
                    in
                    ( Value maybeDocs avalue, end )
                )
        ]


chompMatchingName : Name -> P.Parser E.DeclDef (A.Located Name)
chompMatchingName expectedName =
    P.located (Var.lower E.DeclDefNameRepeat)
        |> andThen
            (\(A.At _ name) ->
                if name == expectedName then
                    succeed (A.At (A.Region P.getPosition P.getPosition) name)

                else
                    Advanced.problem (E.DeclDefNameMatch name)
            )



-- TYPE DECLARATIONS


typeDecl : Maybe Src.Comment -> A.Position -> Space.Parser E.Decl Decl
typeDecl maybeDocs start =
    Keyword.type_ E.DeclStart
        |> andThen
            (\_ ->
                Space.chompAndCheckIndent E.DT_Space E.DT_IndentName
                    |> andThen
                        (\_ ->
                            oneOf
                                [ Keyword.alias_ E.DT_Name
                                    |> andThen
                                        (\_ ->
                                            Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
                                                |> andThen (\_ -> chompAliasNameToEquals)
                                                |> andThen
                                                    (\( name, args ) ->
                                                        Type.expression
                                                            |> map
                                                                (\( tipe, end ) ->
                                                                    let
                                                                        alias_ : A.Located Src.Alias
                                                                        alias_ =
                                                                            A.at start end (Src.Alias name args tipe)
                                                                    in
                                                                    ( Alias maybeDocs alias_, end )
                                                                )
                                                    )
                                        )
                                , chompCustomNameToEquals
                                    |> andThen
                                        (\( name, args ) ->
                                            Type.variant
                                                |> andThen
                                                    (\( firstVariant, firstEnd ) ->
                                                        chompVariants [ firstVariant ] firstEnd
                                                            |> map
                                                                (\( variants, end ) ->
                                                                    let
                                                                        union : A.Located Src.Union
                                                                        union =
                                                                            A.at start end (Src.Union name args variants)
                                                                    in
                                                                    ( Union maybeDocs union, end )
                                                                )
                                                    )
                                        )
                                ]
                        )
            )



-- TYPE ALIASES


chompAliasNameToEquals : P.Parser E.TypeAlias ( A.Located Name, List (A.Located Name) )
chompAliasNameToEquals =
    P.located (Var.upper E.AliasName)
        |> andThen
            (\name ->
                Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
                    |> andThen (\_ -> chompAliasNameToEqualsHelp name [])
            )


chompAliasNameToEqualsHelp : A.Located Name -> List (A.Located Name) -> P.Parser E.TypeAlias ( A.Located Name, List (A.Located Name) )
chompAliasNameToEqualsHelp name args =
    oneOf
        [ P.located (Var.lower E.AliasEquals)
            |> andThen
                (\arg ->
                    Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
                        |> andThen (\_ -> chompAliasNameToEqualsHelp name (arg :: args))
                )
        , Advanced.symbol "=" E.AliasEquals
            |> andThen (\_ -> Space.chompAndCheckIndent E.AliasSpace E.AliasIndentBody)
            |> map (\_ -> ( name, List.reverse args ))
        ]



-- CUSTOM TYPES


chompCustomNameToEquals : P.Parser E.CustomType ( A.Located Name, List (A.Located Name) )
chompCustomNameToEquals =
    P.located (Var.upper E.CT_Name)
        |> andThen
            (\name ->
                Space.chompAndCheckIndent E.CT_Space E.CT_IndentEquals
                    |> andThen (\_ -> chompCustomNameToEqualsHelp name [])
            )


chompCustomNameToEqualsHelp : A.Located Name -> List (A.Located Name) -> P.Parser E.CustomType ( A.Located Name, List (A.Located Name) )
chompCustomNameToEqualsHelp name args =
    oneOf
        [ P.located (Var.lower E.CT_Equals)
            |> andThen
                (\arg ->
                    Space.chompAndCheckIndent E.CT_Space E.CT_IndentEquals
                        |> andThen (\_ -> chompCustomNameToEqualsHelp name (arg :: args))
                )
        , Advanced.symbol "=" E.CT_Equals
            |> andThen (\_ -> Space.chompAndCheckIndent E.CT_Space E.CT_IndentAfterEquals)
            |> map (\_ -> ( name, List.reverse args ))
        ]


chompVariants : List ( A.Located Name, List Src.Type ) -> A.Position -> Space.Parser E.CustomType (List ( A.Located Name, List Src.Type ))
chompVariants variants end =
    oneOf
        [ Space.checkIndent end E.CT_IndentBar
            |> andThen (\_ -> Advanced.symbol "|" E.CT_Bar)
            |> andThen (\_ -> Space.chompAndCheckIndent E.CT_Space E.CT_IndentAfterBar)
            |> andThen (\_ -> Type.variant)
            |> andThen (\( variant, newEnd ) -> chompVariants (variant :: variants) newEnd)
        ]
        |> oneOfWithFallback ( List.reverse variants, end )



-- PORT


portDecl : Maybe Src.Comment -> Space.Parser E.Decl Decl
portDecl maybeDocs =
    Keyword.port_ E.DeclStart
        |> andThen
            (\_ ->
                Space.chompAndCheckIndent E.PortSpace E.PortIndentName
                    |> andThen (\_ -> P.located (Var.lower E.PortName))
                    |> andThen
                        (\name ->
                            Space.chompAndCheckIndent E.PortSpace E.PortIndentColon
                                |> andThen (\_ -> Advanced.symbol ":" E.PortColon)
                                |> andThen (\_ -> Space.chompAndCheckIndent E.PortSpace E.PortIndentType)
                                |> andThen
                                    (\_ ->
                                        Type.expression
                                            |> map
                                                (\( tipe, end ) ->
                                                    ( Port maybeDocs (Src.Port name tipe)
                                                    , end
                                                    )
                                                )
                                    )
                        )
            )



-- INFIX


infix_ : P.Parser E.Module (A.Located Src.Infix)
infix_ =
    P.getPosition
        |> andThen
            (\start ->
                Keyword.infix_ E.Infix
                    |> andThen (\_ -> Space.chompAndCheckIndent (\_ r c -> E.Infix r c) E.Infix)
                    |> andThen
                        (\_ ->
                            oneOf
                                [ Keyword.left_ E.Infix |> map (\_ -> Binop.Left)
                                , Keyword.right_ E.Infix |> map (\_ -> Binop.Right)
                                , Keyword.non_ E.Infix |> map (\_ -> Binop.Non)
                                ]
                        )
                    |> andThen
                        (\associativity ->
                            Space.chompAndCheckIndent (\_ r c -> E.Infix r c) E.Infix
                                |> andThen (\_ -> Number.precedence E.Infix)
                                |> andThen
                                    (\precedence ->
                                        Space.chompAndCheckIndent (\_ r c -> E.Infix r c) E.Infix
                                            |> andThen (\_ -> Advanced.symbol "(" E.Infix)
                                            |> andThen (\_ -> Symbol.operator E.Infix (\_ r c -> E.Infix r c))
                                            |> andThen
                                                (\op ->
                                                    Advanced.symbol ")" E.Infix
                                                        |> andThen (\_ -> Space.chompAndCheckIndent (\_ r c -> E.Infix r c) E.Infix)
                                                        |> andThen (\_ -> Advanced.symbol "=" E.Infix)
                                                        |> andThen (\_ -> Space.chompAndCheckIndent (\_ r c -> E.Infix r c) E.Infix)
                                                        |> andThen (\_ -> Var.lower E.Infix)
                                                        |> andThen
                                                            (\name ->
                                                                P.getPosition
                                                                    |> andThen
                                                                        (\end ->
                                                                            Space.chomp (\_ r c -> E.Infix r c)
                                                                                |> andThen (\_ -> Space.checkFreshLine E.Infix)
                                                                                |> map (\_ -> A.at start end (Src.Infix op associativity precedence name))
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )

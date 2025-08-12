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


type Decl
    = Value (Maybe Src.Comment) (A.Located Src.Value)
    | Union (Maybe Src.Comment) (A.Located Src.Union)
    | Alias (Maybe Src.Comment) (A.Located Src.Alias)
    | Port (Maybe Src.Comment) Src.Port


declaration : SyntaxVersion -> P.Parser Decl
declaration syntaxVersion =
    chompDocComment
        |> P.andThen
            (\maybeDocs ->
                P.oneOf
                    [ typeDecl maybeDocs
                    , portDecl maybeDocs
                    , valueDecl syntaxVersion maybeDocs
                    ]
            )


chompDocComment : P.Parser (Maybe Src.Comment)
chompDocComment =
    P.oneOfWithFallback
        [ Space.docComment (P.Problem_Decl P.DP_Start) (P.Problem_Decl (P.DP_Space P.HasTab))
            |> P.andThen
                (\docComment ->
                    Space.chomp
                        |> P.andThen (\_ -> Space.checkFreshLine (P.Problem_Decl P.DP_FreshLineAfterDocComment))
                        |> P.map (\_ -> Just docComment)
                )
        ]
        Nothing


valueDecl : SyntaxVersion -> Maybe Src.Comment -> P.Parser Decl
valueDecl syntaxVersion maybeDocs =
    P.addLocation (Var.lower (P.Problem_Decl P.DP_Start))
        |> P.andThen (\name ->
            Space.chomp
                |> P.andThen (\_ ->
                    P.oneOf
                        [ P.succeed ()
                            |. P.word1 ':' (P.Problem_Decl (P.DP_Def (A.toValue name) P.DDP_Equals))
                            |> P.andThen (\_ ->
                                Space.chomp
                                    |> P.andThen (\_ ->
                                        Type.expression
                                            |> P.andThen (\tipe ->
                                                Space.checkFreshLine (P.Problem_Decl (P.DP_Def (A.toValue name) P.DDP_NameRepeat))
                                                    |> P.andThen (\_ -> chompMatchingName (A.toValue name))
                                                    |> P.andThen (\defName ->
                                                        Space.chomp
                                                            |> P.andThen (\_ -> chompDefArgsAndBody syntaxVersion maybeDocs defName (Just tipe) [])
                                                       )
                                               )
                                       )
                               )
                        , chompDefArgsAndBody syntaxVersion maybeDocs name Nothing []
                        ]
                   )
           )


chompDefArgsAndBody : SyntaxVersion -> Maybe Src.Comment -> A.Located Name -> Maybe Src.Type -> List Src.Pattern -> P.Parser Decl
chompDefArgsAndBody syntaxVersion maybeDocs name tipe revArgs =
    P.oneOf
        [ Pattern.term syntaxVersion
            |> P.andThen (\arg ->
                Space.chomp
                    |> P.andThen (\_ -> chompDefArgsAndBody syntaxVersion maybeDocs name tipe (arg :: revArgs))
               )
        , P.succeed ()
            |. P.word1 '=' (P.Problem_Decl (P.DP_Def (A.toValue name) P.DDP_Equals))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        Expr.expression syntaxVersion
                            |> P.map (\body ->
                                let
                                    value : Src.Value
                                    value =
                                        Src.Value name (List.reverse revArgs) body tipe

                                    avalue : A.Located Src.Value
                                    avalue =
                                        A.merge name body value
                                in
                                Value maybeDocs avalue
                               )
                       )
               )
        ]


chompMatchingName : Name -> P.Parser (A.Located Name)
chompMatchingName expectedName =
    P.addLocation (Var.lower (P.Problem_Decl (P.DP_Def expectedName P.DDP_NameRepeat)))
        |> P.andThen (\locatedName ->
            if A.toValue locatedName == expectedName then
                P.succeed locatedName
            else
                P.problem (P.Problem_Decl (P.DP_Def expectedName (P.DP_NameMatch (A.toValue locatedName))))
           )


typeDecl : Maybe Src.Comment -> P.Parser Decl
typeDecl maybeDocs =
    P.inContext (P.CtxNode P.NDeclType)
        (P.succeed ()
            |. Keyword.type_ (P.Problem_Decl P.DP_Start)
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        P.oneOf
                            [ P.inContext (P.CtxNode P.NDeclTypeAlias)
                                (P.succeed ()
                                    |. Keyword.alias_ (P.Problem_Decl (P.DP_Type P.DTP_Name))
                                    |> P.andThen (\_ ->
                                        Space.chomp
                                            |> P.andThen (\_ -> chompAliasNameToEquals)
                                            |> P.andThen (\( name, args ) ->
                                                Type.expression
                                                    |> P.map (\tipe ->
                                                        Alias maybeDocs (A.merge name tipe (Src.Alias name args tipe))
                                                       )
                                               )
                                       )
                                )
                            , chompCustomNameToEquals
                                |> P.andThen (\( name, args ) ->
                                    Type.variant
                                        |> P.andThen (\firstVariant ->
                                            chompVariants [ firstVariant ]
                                                |> P.map (\variants ->
                                                    Union maybeDocs (A.merge name (List.head variants |> Maybe.map Tuple.first |> Maybe.withDefault (A.sameAs name "")) (Src.Union name args variants))
                                                   )
                                           )
                                   )
                            ]
                       )
               )
        )


chompAliasNameToEquals : P.Parser ( A.Located Name, List (A.Located Name) )
chompAliasNameToEquals =
    P.addLocation (Var.upper (P.Problem_Decl (P.DP_Type (P.DTP_Alias P.TAP_Name))))
        |> P.andThen (\name ->
            Space.chomp
                |> P.andThen (\_ -> chompAliasNameToEqualsHelp name [])
           )


chompAliasNameToEqualsHelp : A.Located Name -> List (A.Located Name) -> P.Parser ( A.Located Name, List (A.Located Name) )
chompAliasNameToEqualsHelp name args =
    P.oneOf
        [ P.addLocation (Var.lower (P.Problem_Decl (P.DP_Type (P.DTP_Alias P.TAP_Equals))))
            |> P.andThen (\arg ->
                Space.chomp
                    |> P.andThen (\_ -> chompAliasNameToEqualsHelp name (arg :: args))
               )
        , P.succeed ( name, List.reverse args )
            |. P.word1 '=' (P.Problem_Decl (P.DP_Type (P.DTP_Alias P.TAP_Equals)))
            |> P.andThen (\_ -> Space.chomp)
        ]


chompCustomNameToEquals : P.Parser ( A.Located Name, List (A.Located Name) )
chompCustomNameToEquals =
    P.addLocation (Var.upper (P.Problem_Decl (P.DP_Type (P.DTP_Union P.CTP_Name))))
        |> P.andThen (\name ->
            Space.chomp
                |> P.andThen (\_ -> chompCustomNameToEqualsHelp name [])
           )


chompCustomNameToEqualsHelp : A.Located Name -> List (A.Located Name) -> P.Parser ( A.Located Name, List (A.Located Name) )
chompCustomNameToEqualsHelp name args =
    P.oneOf
        [ P.addLocation (Var.lower (P.Problem_Decl (P.DP_Type (P.DTP_Union P.CTP_Equals))))
            |> P.andThen (\arg ->
                Space.chomp
                    |> P.andThen (\_ -> chompCustomNameToEqualsHelp name (arg :: args))
               )
        , P.succeed ( name, List.reverse args )
            |. P.word1 '=' (P.Problem_Decl (P.DP_Type (P.DTP_Union P.CTP_Equals)))
            |> P.andThen (\_ -> Space.chomp)
        ]


chompVariants : List ( A.Located Name, List Src.Type ) -> P.Parser (List ( A.Located Name, List Src.Type ))
chompVariants variants =
    P.oneOfWithFallback
        [ P.succeed ()
            |. P.word1 '|' (P.Problem_Decl (P.DP_Type (P.DTP_Union P.CTP_Bar)))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        Type.variant
                            |> P.andThen (\variant -> chompVariants (variant :: variants))
                       )
               )
        ]
        (List.reverse variants)


portDecl : Maybe Src.Comment -> P.Parser Decl
portDecl maybeDocs =
    P.inContext (P.CtxNode P.NPort)
        (P.succeed ()
            |. Keyword.port_ (P.Problem_Decl P.DP_Start)
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ -> P.addLocation (Var.lower (P.Problem_Decl (P.DP_Port P.PP_Name))))
                    |> P.andThen (\name ->
                        Space.chomp
                            |> P.andThen (\_ ->
                                P.succeed ()
                                    |. P.word1 ':' (P.Problem_Decl (P.DP_Port P.PP_Colon))
                                    |> P.andThen (\_ ->
                                        Space.chomp
                                            |> P.andThen (\_ ->
                                                Type.expression
                                                    |> P.map (\tipe -> Port maybeDocs (Src.Port name tipe))
                                               )
                                       )
                               )
                       )
               )
        )


infix_ : P.Parser (A.Located Src.Infix)
infix_ =
    P.addLocation
        (P.succeed ()
            |. Keyword.infix_ (P.Problem_Module P.MP_Infix)
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        P.oneOf
                            [ P.succeed Binop.Left |. Keyword.left_ (P.Problem_Module P.MP_Infix)
                            , P.succeed Binop.Right |. Keyword.right_ (P.Problem_Module P.MP_Infix)
                            , P.succeed Binop.Non |. Keyword.non_ (P.Problem_Module P.MP_Infix)
                            ]
                            |> P.andThen (\associativity ->
                                Space.chomp
                                    |> P.andThen (\_ -> Number.precedence (P.Problem_Module P.MP_Infix))
                                    |> P.andThen (\precedence ->
                                        Space.chomp
                                            |> P.andThen (\_ ->
                                                P.succeed ()
                                                    |. P.word1 '(' (P.Problem_Module P.MP_Infix)
                                                    |> P.andThen (\_ -> Symbol.operator (P.Problem_Module P.MP_Infix) (\op -> P.Problem_Module P.MP_Infix))
                                                    |> P.andThen (\op ->
                                                        P.succeed ()
                                                            |. P.word1 ')' (P.Problem_Module P.MP_Infix)
                                                            |> P.andThen (\_ ->
                                                                Space.chomp
                                                                    |> P.andThen (\_ ->
                                                                        P.succeed ()
                                                                            |. P.word1 '=' (P.Problem_Module P.MP_Infix)
                                                                            |> P.andThen (\_ ->
                                                                                Space.chomp
                                                                                    |> P.andThen (\_ ->
                                                                                        Var.lower (P.Problem_Module P.MP_Infix)
                                                                                            |> P.andThen (\name ->
                                                                                                Space.chomp
                                                                                                    |> P.andThen (\_ ->
                                                                                                        Space.checkFreshLine (P.Problem_Module P.MP_Infix)
                                                                                                            |> P.map (\_ -> Src.Infix op associativity precedence name)
                                                                                                       )
                                                                                               )
                                                                                       )
                                                                               )
                                                                       )
                                                               )
                                                       )
                                               )
                                       )
                               )
                       )
               )
        )

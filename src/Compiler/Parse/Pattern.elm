module Compiler.Parse.Pattern exposing
    ( expression
    , term
    )

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.NewPrimitives as P
import Compiler.Parse.Number as Number
import Compiler.Parse.Space as Space
import Compiler.Parse.String as String
import Compiler.Parse.SyntaxVersion as SV exposing (SyntaxVersion)
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Parser exposing (..)
import Parser.Advanced as Advanced



-- TERM


term : SyntaxVersion -> P.Parser E.Pattern Src.Pattern
term syntaxVersion =
    oneOf
        [ P.located (wildcard syntaxVersion |> map Src.PAnything)
        , P.located (Var.lower E.PStart |> map Src.PVar)
        , P.located (constructor syntaxVersion)
        , P.located (Number.number E.PStart E.PNumber |> andThen numberToPattern)
        , P.located (String.string E.PStart E.PString |> map Src.PStr)
        , P.located (String.character E.PStart E.PChar |> map Src.PChr)
        , record
        , tuple syntaxVersion
        , list syntaxVersion
        ]


constructor : SyntaxVersion -> P.Parser E.Pattern Src.Pattern_
constructor syntaxVersion =
    P.getPosition
        |> andThen
            (\start ->
                Var.foreignUpper E.PStart
                    |> andThen
                        (\upper ->
                            P.getPosition
                                |> andThen
                                    (\end ->
                                        let
                                            region =
                                                A.Region start end
                                        in
                                        case upper of
                                            Var.Unqualified name ->
                                                succeed (Src.PCtor region name [])

                                            Var.Qualified home name ->
                                                succeed (Src.PCtorQual region home name [])
                                    )
                        )
            )


numberToPattern : Number.Number -> P.Parser E.Pattern Src.Pattern_
numberToPattern number =
    case number of
        Number.Int int ->
            succeed (Src.PInt int)

        Number.Float float ->
            Advanced.problem (E.PFloat (String.fromFloat float |> String.length))



-- WILDCARD


wildcard : SyntaxVersion -> P.Parser E.Pattern Name.Name
wildcard syntaxVersion =
    Advanced.symbol "_" E.PStart
        |> andThen
            (\_ ->
                oneOf
                    [ -- Guida-style wildcard with name
                      P.getPosition |> andThen (\_ -> Var.lower E.PWildcardNotVar)
                    , -- Plain wildcard
                      succeed ""
                    ]
            )



-- RECORDS


record : P.Parser E.Pattern Src.Pattern
record =
    P.located
        (Advanced.symbol "{" E.PStart
            |> andThen
                (\_ ->
                    oneOf
                        [ Advanced.symbol "}" E.PStart |> andThen (\_ -> succeed [])
                        , loop [] recordFieldHelp
                        ]
                )
            |> map Src.PRecord
        )


recordFieldHelp : List (A.Located Name.Name) -> P.Parser E.PRecord (Step (List (A.Located Name.Name)) (List (A.Located Name.Name)))
recordFieldHelp revFields =
    oneOf
        [ P.located (Var.lower E.PRecordField) |> andThen (\var -> succeed (Loop (var :: revFields)))
        , Advanced.symbol "," E.PRecordEnd |> andThen (\_ -> succeed (Loop revFields))
        , Advanced.symbol "}" E.PRecordEnd |> andThen (\_ -> succeed (Done (List.reverse revFields)))
        ]



-- TUPLES


tuple : SyntaxVersion -> P.Parser E.Pattern Src.Pattern
tuple syntaxVersion =
    P.located
        (Advanced.symbol "(" E.PStart
            |> andThen
                (\_ ->
                    oneOf
                        [ Advanced.symbol ")" E.PStart |> andThen (\_ -> succeed Src.PUnit)
                        , tupleHelp syntaxVersion
                        ]
                )
        )


tupleHelp : SyntaxVersion -> P.Parser E.Pattern Src.Pattern_
tupleHelp syntaxVersion =
    expression syntaxVersion
        |> andThen
            (\( first, _ ) ->
                loop [ first ] (tupleRestHelp syntaxVersion)
                    |> andThen
                        (\patterns ->
                            case patterns of
                                [ p ] ->
                                    succeed (A.toValue p)

                                p1 :: p2 :: rest ->
                                    succeed (Src.PTuple p1 p2 rest)

                                [] ->
                                    -- This should not happen
                                    Advanced.problem E.PTupleOpen
                        )
            )


tupleRestHelp : SyntaxVersion -> List Src.Pattern -> P.Parser E.PTuple (Step (List Src.Pattern) (List Src.Pattern))
tupleRestHelp syntaxVersion revPatterns =
    oneOf
        [ Advanced.symbol "," E.PTupleEnd
            |> andThen (\_ -> expression syntaxVersion |> andThen (\( p, _ ) -> succeed (Loop (p :: revPatterns))))
        , Advanced.symbol ")" E.PTupleEnd
            |> andThen (\_ -> succeed (Done (List.reverse revPatterns)))
        ]



-- LIST


list : SyntaxVersion -> P.Parser E.Pattern Src.Pattern
list syntaxVersion =
    P.located
        (Advanced.symbol "[" E.PStart
            |> andThen
                (\_ ->
                    oneOf
                        [ Advanced.symbol "]" E.PStart |> andThen (\_ -> succeed [])
                        , loop [] (listElmHelp syntaxVersion)
                        ]
                )
            |> map Src.PList
        )


listElmHelp : SyntaxVersion -> List Src.Pattern -> P.Parser E.PList (Step (List Src.Pattern) (List Src.Pattern))
listElmHelp syntaxVersion revPatterns =
    oneOf
        [ expression syntaxVersion |> andThen (\( p, _ ) -> succeed (Loop (p :: revPatterns)))
        , Advanced.symbol "," E.PListEnd |> andThen (\_ -> succeed (Loop revPatterns))
        , Advanced.symbol "]" E.PListEnd |> andThen (\_ -> succeed (Done (List.reverse revPatterns)))
        ]



-- EXPRESSION


expression : SyntaxVersion -> Space.Parser E.Pattern Src.Pattern
expression syntaxVersion =
    P.getPosition
        |> andThen
            (\start ->
                exprPart syntaxVersion
                    |> andThen
                        (\ePart ->
                            exprHelp syntaxVersion start [] ePart
                        )
            )


exprHelp : SyntaxVersion -> A.Position -> List Src.Pattern -> ( Src.Pattern, A.Position ) -> Space.Parser E.Pattern Src.Pattern
exprHelp syntaxVersion start revPatterns ( pattern, end ) =
    oneOf
        [ Space.checkIndent end E.PIndentStart
            |> andThen (\_ -> Advanced.symbol "::" E.PStart)
            |> andThen (\_ -> Space.chompAndCheckIndent E.PSpace E.PIndentStart)
            |> andThen (\_ -> exprPart syntaxVersion)
            |> andThen (\ePart -> exprHelp syntaxVersion start (pattern :: revPatterns) ePart)
        , Space.checkIndent end E.PIndentStart
            |> andThen (\_ -> Keyword.as_ E.PStart)
            |> andThen (\_ -> Space.chompAndCheckIndent E.PSpace E.PIndentAlias)
            |> andThen (\_ -> P.located (Var.lower E.PAlias))
            |> andThen
                (\alias_ ->
                    P.getPosition
                        |> andThen
                            (\newEnd ->
                                Space.chomp E.PSpace
                                    |> map
                                        (\_ ->
                                            ( A.at start newEnd (Src.PAlias (List.foldl cons pattern revPatterns) alias_)
                                            , newEnd
                                            )
                                        )
                            )
                )
        ]
        |> oneOfWithFallback ( List.foldl cons pattern revPatterns, end )


cons : Src.Pattern -> Src.Pattern -> Src.Pattern
cons hd tl =
    A.merge hd tl (Src.PCons hd tl)



-- EXPRESSION PART


exprPart : SyntaxVersion -> Space.Parser E.Pattern Src.Pattern
exprPart syntaxVersion =
    oneOf
        [ P.located (constructorWithArgs syntaxVersion)
        , term syntaxVersion |> andThen (\t -> P.getPosition |> andThen (\p -> succeed ( t, p )))
        ]


constructorWithArgs : SyntaxVersion -> P.Parser E.Pattern Src.Pattern_
constructorWithArgs syntaxVersion =
    P.getPosition
        |> andThen
            (\start ->
                Var.foreignUpper E.PStart
                    |> andThen
                        (\upper ->
                            loop [] (term syntaxVersion |> andThen (\arg -> succeed (Loop [ arg ])))
                                |> andThen
                                    (\args ->
                                        P.getPosition
                                            |> andThen
                                                (\end ->
                                                    let
                                                        region =
                                                            A.Region start end
                                                    in
                                                    case upper of
                                                        Var.Unqualified name ->
                                                            succeed (Src.PCtor region name args)

                                                        Var.Qualified home name ->
                                                            succeed (Src.PCtorQual region home name args)
                                                )
                                    )
                        )
            )

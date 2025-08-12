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


term : SyntaxVersion -> P.Parser Src.Pattern
term syntaxVersion =
    P.oneOf
        [ record
        , tuple syntaxVersion
        , list syntaxVersion
        , termHelp syntaxVersion
        ]


termHelp : SyntaxVersion -> P.Parser Src.Pattern
termHelp syntaxVersion =
    P.oneOf
        [ P.addLocation (wildcard syntaxVersion)
            |> P.map (A.map Src.PAnything)
        , P.addLocation (Var.lower (P.Problem_Pattern P.PP_Start))
            |> P.map (A.map Src.PVar)
        , Var.foreignUpper (P.Problem_Pattern P.PP_Start)
            |> P.andThen (\upper ->
                P.addLocation (P.succeed upper)
                    |> P.map (\locatedUpper ->
                        let
                            region =
                                A.toRegion locatedUpper
                        in
                        A.at region region <|
                            case upper of
                                Var.Unqualified name ->
                                    Src.PCtor region name []

                                Var.Qualified home name ->
                                    Src.PCtorQual region home name []
                    )
            )
        , Number.number
            |> P.andThen (\number ->
                P.addLocation (P.succeed number)
                    |> P.andThen (\locatedNumber ->
                        case number of
                            Number.Int int ->
                                P.succeed (A.map (\_ -> Src.PInt int) locatedNumber)

                            Number.Float float ->
                                P.problem (P.Problem_Pattern (P.PP_Float (String.fromFloat float |> String.length)))
                    )
            )
        , String.string
            |> P.andThen (\str -> P.addLocation (P.succeed str) |> P.map (A.map Src.PStr))
        , String.character
            |> P.andThen (\chr -> P.addLocation (P.succeed chr) |> P.map (A.map Src.PChr))
        ]



-- WILDCARD


wildcard : SyntaxVersion -> P.Parser Name.Name
wildcard syntaxVersion =
    P.succeed ()
        |. P.word1 '_' (P.Problem_Pattern P.PP_Start)
        |> P.andThen (\_ ->
            P.getChompedString (P.chompWhile (\c -> Char.isAlphaNum c || c == '_'))
                |> P.andThen (\rest ->
                    if String.isEmpty rest then
                        P.succeed ""
                    else
                        case syntaxVersion of
                            SV.Elm ->
                                P.problem (P.Problem_Pattern (P.PP_WildcardNotVar ("_" ++ rest) (String.length rest + 1)))

                            SV.Guida ->
                                if Var.isReservedWord rest then
                                    P.problem (P.Problem_Pattern (P.PP_WildcardReservedWord rest (String.length rest + 1)))
                                else
                                    P.succeed rest
                   )
           )



-- RECORDS


record : P.Parser Src.Pattern
record =
    P.inContext (P.CtxNode P.NRecord)
        (P.succeed ()
            |. P.word1 '{' (P.Problem_Pattern (P.PP_Record P.PRP_Open))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        P.oneOf
                            [ P.addLocation (Var.lower (P.Problem_Pattern (P.PP_Record P.PRP_Field)))
                                |> P.andThen (\var ->
                                    Space.chomp
                                        |> P.andThen (\_ -> recordHelp [ var ])
                                   )
                            , P.succeed (Src.PRecord [])
                                |. P.word1 '}' (P.Problem_Pattern (P.PP_Record P.PRP_End))
                            ]
                       )
               )
            |> P.map A.toLocated
        )


recordHelp : List (A.Located Name.Name) -> P.Parser Src.Pattern_
recordHelp vars =
    P.oneOf
        [ P.succeed ()
            |. P.word1 ',' (P.Problem_Pattern (P.PP_Record P.PRP_End))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        P.addLocation (Var.lower (P.Problem_Pattern (P.PP_Record P.PRP_Field)))
                            |> P.andThen (\var ->
                                Space.chomp
                                    |> P.andThen (\_ -> recordHelp (var :: vars))
                               )
                       )
               )
        , P.succeed ()
            |. P.word1 '}' (P.Problem_Pattern (P.PP_Record P.PRP_End))
            |> P.map (\_ -> Src.PRecord (List.reverse vars))
        ]



-- TUPLES


tuple : SyntaxVersion -> P.Parser Src.Pattern
tuple syntaxVersion =
    P.inContext (P.CtxNode P.NParens)
        (P.succeed ()
            |. P.word1 '(' (P.Problem_Pattern (P.PP_Tuple P.PTP_Open))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        P.oneOf
                            [ expression syntaxVersion
                                |> P.andThen (\pattern -> tupleHelp syntaxVersion pattern [])
                            , P.succeed Src.PUnit
                                |. P.word1 ')' (P.Problem_Pattern (P.PP_Tuple P.PTP_End))
                            ]
                       )
               )
            |> P.map A.toLocated
        )


tupleHelp : SyntaxVersion -> Src.Pattern -> List Src.Pattern -> P.Parser Src.Pattern_
tupleHelp syntaxVersion firstPattern revPatterns =
    P.oneOf
        [ P.succeed ()
            |. P.word1 ',' (P.Problem_Pattern (P.PP_Tuple P.PTP_End))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        expression syntaxVersion
                            |> P.andThen (\pattern -> tupleHelp syntaxVersion firstPattern (pattern :: revPatterns))
                       )
               )
        , P.succeed ()
            |. P.word1 ')' (P.Problem_Pattern (P.PP_Tuple P.PTP_End))
            |> P.map (\_ ->
                case List.reverse revPatterns of
                    [] ->
                        A.toValue firstPattern

                    secondPattern :: otherPatterns ->
                        Src.PTuple firstPattern secondPattern otherPatterns
               )
        ]



-- LIST


list : SyntaxVersion -> P.Parser Src.Pattern
list syntaxVersion =
    P.inContext (P.CtxNode P.NList)
        (P.succeed ()
            |. P.word1 '[' (P.Problem_Pattern (P.PP_List P.PLP_Open))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        P.oneOf
                            [ expression syntaxVersion
                                |> P.andThen (\pattern -> listHelp syntaxVersion [ pattern ])
                            , P.succeed (Src.PList [])
                                |. P.word1 ']' (P.Problem_Pattern (P.PP_List P.PLP_End))
                            ]
                       )
               )
            |> P.map A.toLocated
        )


listHelp : SyntaxVersion -> List Src.Pattern -> P.Parser Src.Pattern_
listHelp syntaxVersion patterns =
    P.oneOf
        [ P.succeed ()
            |. P.word1 ',' (P.Problem_Pattern (P.PP_List P.PLP_End))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        expression syntaxVersion
                            |> P.andThen (\pattern -> listHelp syntaxVersion (pattern :: patterns))
                       )
               )
        , P.succeed ()
            |. P.word1 ']' (P.Problem_Pattern (P.PP_List P.PLP_End))
            |> P.map (\_ -> Src.PList (List.reverse patterns))
        ]



-- EXPRESSION


expression : SyntaxVersion -> P.Parser Src.Pattern
expression syntaxVersion =
    exprPart syntaxVersion
        |> P.andThen (\pattern -> exprHelp syntaxVersion pattern)


exprHelp : SyntaxVersion -> Src.Pattern -> P.Parser Src.Pattern
exprHelp syntaxVersion pattern =
    P.oneOfWithFallback
        [ P.succeed ()
            |. P.word2 ':' ':' (P.Problem_Pattern P.PP_Start)
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        exprPart syntaxVersion
                            |> P.andThen (\hd -> exprHelp syntaxVersion (cons hd pattern))
                       )
               )
        , Keyword.as_ (P.Problem_Pattern P.PP_Start)
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        P.addLocation (Var.lower (P.Problem_Pattern P.PP_Alias))
                            |> P.andThen (\alias_ ->
                                P.succeed (A.merge pattern alias_ (Src.PAlias pattern alias_))
                               )
                       )
               )
        ]
        pattern


cons : Src.Pattern -> Src.Pattern -> Src.Pattern
cons hd tl =
    A.merge hd tl (Src.PCons hd tl)



-- EXPRESSION PART


exprPart : SyntaxVersion -> P.Parser Src.Pattern
exprPart syntaxVersion =
    P.oneOf
        [ Var.foreignUpper (P.Problem_Pattern P.PP_Start)
            |> P.andThen (\upper ->
                P.addLocation (P.succeed upper)
                    |> P.andThen (\locatedUpper ->
                        exprTermHelp syntaxVersion (A.toRegion locatedUpper) upper []
                       )
               )
        , term syntaxVersion
        ]


exprTermHelp : SyntaxVersion -> A.Region -> Var.Upper -> List Src.Pattern -> P.Parser Src.Pattern
exprTermHelp syntaxVersion region upper revArgs =
    Space.chomp
        |> P.andThen (\_ ->
            P.oneOfWithFallback
                [ term syntaxVersion
                    |> P.andThen (\arg -> exprTermHelp syntaxVersion region upper (arg :: revArgs))
                ]
                (A.at region region <|
                    case upper of
                        Var.Unqualified name ->
                            Src.PCtor region name (List.reverse revArgs)

                        Var.Qualified home name ->
                            Src.PCtorQual region home name (List.reverse revArgs)
                )
           )

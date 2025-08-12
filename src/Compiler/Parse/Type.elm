module Compiler.Parse.Type exposing
    ( expression
    , variant
    )

import Compiler.AST.Source as Src
import Compiler.Data.Name exposing (Name)
import Compiler.Parse.NewPrimitives as P
import Compiler.Parse.Space as Space
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A


term : P.Parser Src.Type
term =
    P.oneOf
        [ Var.foreignUpper (P.Problem_Type P.TP_Start)
            |> P.andThen (\upper ->
                P.addLocation (P.succeed upper)
                    |> P.map (\locatedUpper ->
                        let
                            region =
                                A.toRegion locatedUpper
                        in
                        case upper of
                            Var.Unqualified name ->
                                A.At region (Src.TType region name [])

                            Var.Qualified home name ->
                                A.At region (Src.TTypeQual region home name [])
                    )
            )
        , Var.lower (P.Problem_Type P.TP_Start)
            |> P.andThen (\var -> P.addLocation (P.succeed var) |> P.map (A.map Src.TVar))
        , P.inContext (P.CtxNode P.NParens)
            (P.succeed ()
                |. P.word1 '(' (P.Problem_Type (P.TP_Tuple P.TTP_Open))
                |> P.andThen (\_ ->
                    P.oneOf
                        [ P.succeed Src.TUnit |. P.word1 ')' (P.Problem_Type (P.TP_Tuple P.TTP_End))
                        , Space.chomp
                            |> P.andThen (\_ ->
                                expression
                                    |> P.andThen (\tipe -> chompTupleEnd tipe [])
                               )
                        ]
                        |> P.map A.toLocated
                   )
            )
        , P.inContext (P.CtxNode P.NRecord)
            (P.succeed ()
                |. P.word1 '{' (P.Problem_Type (P.TP_Record P.TRP_Open))
                |> P.andThen (\_ ->
                    Space.chomp
                        |> P.andThen (\_ ->
                            P.oneOf
                                [ P.succeed (Src.TRecord [] Nothing) |. P.word1 '}' (P.Problem_Type (P.TP_Record P.TRP_End))
                                , P.addLocation (Var.lower (P.Problem_Type (P.TP_Record P.TRP_Field)))
                                    |> P.andThen (\name ->
                                        Space.chomp
                                            |> P.andThen (\_ ->
                                                P.oneOf
                                                    [ P.succeed ()
                                                        |. P.word1 '|' (P.Problem_Type (P.TP_Record P.TRP_Colon))
                                                        |> P.andThen (\_ ->
                                                            Space.chomp
                                                                |> P.andThen (\_ ->
                                                                    chompField
                                                                        |> P.andThen (\field ->
                                                                            chompRecordEnd [ field ]
                                                                                |> P.map (\fields -> Src.TRecord fields (Just name))
                                                                        )
                                                                )
                                                        )
                                                    , P.succeed ()
                                                        |. P.word1 ':' (P.Problem_Type (P.TP_Record P.TRP_Colon))
                                                        |> P.andThen (\_ ->
                                                            Space.chomp
                                                                |> P.andThen (\_ ->
                                                                    expression
                                                                        |> P.andThen (\tipe ->
                                                                            chompRecordEnd [ ( name, tipe ) ]
                                                                                |> P.map (\fields -> Src.TRecord fields Nothing)
                                                                        )
                                                                )
                                                        )
                                                    ]
                                           )
                                   )
                                ]
                           )
                   )
                |> P.map A.toLocated
            )
        ]


expression : P.Parser Src.Type
expression =
    P.oneOf
        [ app
        , term
        ]
        |> P.andThen (\tipe1 ->
            P.oneOfWithFallback
                [ P.succeed ()
                    |. P.word2 '-' '>' (P.Problem_Type P.TP_Start)
                    |> P.andThen (\_ ->
                        Space.chomp
                            |> P.andThen (\_ ->
                                expression
                                    |> P.map (\tipe2 -> A.merge tipe1 tipe2 (Src.TLambda tipe1 tipe2))
                               )
                       )
                ]
                tipe1
           )


app : P.Parser Src.Type
app =
    Var.foreignUpper (P.Problem_Type P.TP_Start)
        |> P.andThen (\upper ->
            P.addLocation (P.succeed upper)
                |> P.andThen (\locatedUpper ->
                    Space.chomp
                        |> P.andThen (\_ ->
                            chompArgs []
                                |> P.map (\args ->
                                    let
                                        region =
                                            A.toRegion locatedUpper
                                    in
                                    A.at region region <|
                                        case upper of
                                            Var.Unqualified name ->
                                                Src.TType region name args

                                            Var.Qualified home name ->
                                                Src.TTypeQual region home name args
                                )
                           )
                   )
           )


chompArgs : List Src.Type -> P.Parser (List Src.Type)
chompArgs args =
    P.oneOfWithFallback
        [ term
            |> P.andThen (\arg ->
                Space.chomp
                    |> P.andThen (\_ -> chompArgs (arg :: args))
               )
        ]
        (List.reverse args)


chompTupleEnd : Src.Type -> List Src.Type -> P.Parser Src.Type_
chompTupleEnd firstType revTypes =
    P.oneOf
        [ P.succeed ()
            |. P.word1 ',' (P.Problem_Type (P.TP_Tuple P.TTP_End))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        expression
                            |> P.andThen (\tipe -> chompTupleEnd firstType (tipe :: revTypes))
                       )
               )
        , P.succeed ()
            |. P.word1 ')' (P.Problem_Type (P.TP_Tuple P.TTP_End))
            |> P.map (\_ ->
                case List.reverse revTypes of
                    [] ->
                        A.toValue firstType

                    secondType :: otherTypes ->
                        Src.TTuple firstType secondType otherTypes
               )
        ]


type alias Field =
    ( A.Located Name, Src.Type )


chompRecordEnd : List Field -> P.Parser (List Field)
chompRecordEnd fields =
    P.oneOfWithFallback
        [ P.succeed ()
            |. P.word1 ',' (P.Problem_Type (P.TP_Record P.TRP_End))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        chompField
                            |> P.andThen (\field -> chompRecordEnd (field :: fields))
                       )
               )
        , P.succeed ()
            |. P.word1 '}' (P.Problem_Type (P.TP_Record P.TRP_End))
            |> P.map (\_ -> List.reverse fields)
        ]
        (List.reverse fields)


chompField : P.Parser Field
chompField =
    P.addLocation (Var.lower (P.Problem_Type (P.TP_Record P.TRP_Field)))
        |> P.andThen (\name ->
            Space.chomp
                |> P.andThen (\_ ->
                    P.succeed ()
                        |. P.word1 ':' (P.Problem_Type (P.TP_Record P.TRP_Colon))
                        |> P.andThen (\_ ->
                            Space.chomp
                                |> P.andThen (\_ ->
                                    expression
                                        |> P.map (\tipe -> ( name, tipe ))
                                   )
                           )
                   )
           )


variant : P.Parser ( A.Located Name, List Src.Type )
variant =
    P.addLocation (Var.upper (P.Problem_Type P.TP_Start))
        |> P.andThen (\name ->
            Space.chomp
                |> P.andThen (\_ ->
                    chompArgs []
                        |> P.map (\args -> ( name, args ))
                   )
           )

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
import Compiler.Reporting.Error.Syntax as E
import Parser exposing (..)
import Parser.Advanced as Advanced



-- TYPE TERMS


term : P.Parser E.Type Src.Type
term =
    oneOf
        [ P.located (Var.lower E.TStart |> map Src.TVar)
        , P.located constructor
        , tuple
        , record
        ]


constructor : P.Parser E.Type Src.Type_
constructor =
    P.getPosition
        |> andThen
            (\start ->
                Var.foreignUpper E.TStart
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
                                                succeed (Src.TType region name [])

                                            Var.Qualified home name ->
                                                succeed (Src.TTypeQual region home name [])
                                    )
                        )
            )


tuple : P.Parser E.Type Src.Type
tuple =
    P.located
        (Advanced.symbol "(" E.TStart
            |> andThen
                (\_ ->
                    oneOf
                        [ Advanced.symbol ")" E.TStart |> andThen (\_ -> succeed Src.TUnit)
                        , tupleHelp
                        ]
                )
        )


tupleHelp : P.Parser E.Type Src.Type_
tupleHelp =
    expression
        |> andThen
            (\( first, _ ) ->
                loop [ first ] tupleRestHelp
                    |> andThen
                        (\patterns ->
                            case patterns of
                                [ p ] ->
                                    succeed (A.toValue p)

                                p1 :: p2 :: rest ->
                                    succeed (Src.TTuple p1 p2 rest)

                                [] ->
                                    -- This should not happen
                                    Advanced.problem E.TTupleOpen
                        )
            )


tupleRestHelp : List Src.Type -> P.Parser E.TTuple (Step (List Src.Type) (List Src.Type))
tupleRestHelp revPatterns =
    oneOf
        [ Advanced.symbol "," E.TTupleEnd
            |> andThen (\_ -> expression |> andThen (\( p, _ ) -> succeed (Loop (p :: revPatterns))))
        , Advanced.symbol ")" E.TTupleEnd
            |> andThen (\_ -> succeed (Done (List.reverse revPatterns)))
        ]


record : P.Parser E.Type Src.Type
record =
    P.located
        (Advanced.symbol "{" E.TStart
            |> andThen
                (\_ ->
                    oneOf
                        [ Advanced.symbol "}" E.TRecordEnd |> andThen (\_ -> succeed (Src.TRecord [] Nothing))
                        , recordHelp
                        ]
                )
        )


recordHelp : P.Parser E.Type Src.Type_
recordHelp =
    P.located (Var.lower E.TRecordField)
        |> andThen
            (\name ->
                oneOf
                    [ Advanced.symbol "|" E.TRecordColon
                        |> andThen
                            (\_ ->
                                loop [] recordFieldHelp
                                    |> andThen (\fields -> succeed (Src.TRecord fields (Just name)))
                            )
                    , Advanced.symbol ":" E.TRecordColon
                        |> andThen
                            (\_ ->
                                expression
                                    |> andThen
                                        (\( tipe, _ ) ->
                                            loop [ ( name, tipe ) ] recordRestHelp
                                                |> andThen (\fields -> succeed (Src.TRecord fields Nothing))
                                        )
                            )
                    ]
            )


recordRestHelp : List ( A.Located Name, Src.Type ) -> P.Parser E.TRecord (Step (List ( A.Located Name, Src.Type )) (List ( A.Located Name, Src.Type )))
recordRestHelp revFields =
    oneOf
        [ Advanced.symbol "," E.TRecordEnd
            |> andThen (\_ -> recordField |> andThen (\f -> succeed (Loop (f :: revFields))))
        , Advanced.symbol "}" E.TRecordEnd
            |> andThen (\_ -> succeed (Done (List.reverse revFields)))
        ]


recordField : P.Parser E.TRecord ( A.Located Name, Src.Type )
recordField =
    succeed (\n _ t -> ( n, t ))
        |> andMap (P.located (Var.lower E.TRecordField))
        |> andMap (Advanced.symbol ":" E.TRecordColon)
        |> andMap (expression |> map Tuple.first)



-- TYPE EXPRESSIONS


expression : Space.Parser E.Type Src.Type
expression =
    P.getPosition
        |> andThen
            (\start ->
                oneOf [ app, term |> andThen (\t -> P.getPosition |> andThen (\p -> succeed ( t, p ))) ]
                    |> andThen
                        (\( tipe1, end1 ) ->
                            oneOf
                                [ succeed ()
                                    |> andThen (\_ -> Advanced.symbol "->" E.TStart)
                                    |> andThen (\_ -> expression)
                                    |> andThen
                                        (\( tipe2, end2 ) ->
                                            let
                                                tipe : A.Located Src.Type_
                                                tipe =
                                                    A.at start end2 (Src.TLambda tipe1 tipe2)
                                            in
                                            succeed ( tipe, end2 )
                                        )
                                ]
                                |> oneOfWithFallback ( tipe1, end1 )
                        )
            )



-- TYPE CONSTRUCTORS


app : Space.Parser E.Type Src.Type
app =
    P.getPosition
        |> andThen
            (\start ->
                Var.foreignUpper E.TStart
                    |> andThen
                        (\upper ->
                            P.getPosition
                                |> andThen
                                    (\upperEnd ->
                                        loop [] chompArgsHelp
                                            |> andThen
                                                (\args ->
                                                    P.getPosition
                                                        |> andThen
                                                            (\end ->
                                                                let
                                                                    region =
                                                                        A.Region start upperEnd

                                                                    tipe : Src.Type_
                                                                    tipe =
                                                                        case upper of
                                                                            Var.Unqualified name ->
                                                                                Src.TType region name args

                                                                            Var.Qualified home name ->
                                                                                Src.TTypeQual region home name args
                                                                in
                                                                succeed ( A.at start end tipe, end )
                                                            )
                                                )
                                    )
                        )
            )


chompArgsHelp : List Src.Type -> P.Parser E.Type (Step (List Src.Type) (List Src.Type))
chompArgsHelp revArgs =
    oneOf
        [ term |> andThen (\arg -> succeed (Loop (arg :: revArgs)))
        , succeed (Done (List.reverse revArgs))
        ]



-- VARIANT


variant : Space.Parser E.CustomType ( A.Located Name, List Src.Type )
variant =
    P.located (Var.upper E.CT_Variant)
        |> andThen
            (\name ->
                loop [] chompArgsHelp
                    |> andThen
                        (\args ->
                            P.getPosition
                                |> andThen
                                    (\end ->
                                        succeed ( ( name, args ), end )
                                    )
                        )
            )

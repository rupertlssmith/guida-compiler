module Elm.Constraint exposing
    ( Constraint
    , Error(..)
    , anything
    , check
    , decoder
    , defaultElm
    , encode
    , exactly
    , expand
    , goodElm
    , intersect
    , satisfies
    , toChars
    , untilNextMajor
    , untilNextMinor
    )

import Elm.Version as V
import Json.DecodeX as D exposing (Decoder)
import Json.EncodeX as E exposing (Value)
import Parse.Primitives as P exposing (Col, Row)



-- CONSTRAINTS


type Constraint
    = Range V.Version Op Op V.Version


type Op
    = Less
    | LessOrEqual



-- COMMON CONSTRAINTS


exactly : V.Version -> Constraint
exactly version =
    Range version LessOrEqual LessOrEqual version


anything : Constraint
anything =
    Range V.one LessOrEqual LessOrEqual V.maxVersion



-- TO CHARS


toChars : Constraint -> String
toChars constraint =
    case constraint of
        Range lower lowerOp upperOp upper ->
            V.toChars lower ++ opToChars lowerOp ++ "v" ++ opToChars upperOp ++ V.toChars upper


opToChars : Op -> String
opToChars op =
    case op of
        Less ->
            " < "

        LessOrEqual ->
            " <= "



-- IS SATISFIED


satisfies : Constraint -> V.Version -> Bool
satisfies constraint version =
    case constraint of
        Range lower lowerOp upperOp upper ->
            isLess lowerOp lower version
                && isLess upperOp version upper


isLess : Op -> (V.Version -> V.Version -> Bool)
isLess op =
    case op of
        Less ->
            \lower upper ->
                V.compare lower upper == LT

        LessOrEqual ->
            \lower upper ->
                V.compare lower upper /= GT


check : Constraint -> V.Version -> Order
check constraint version =
    case constraint of
        Range lower lowerOp upperOp upper ->
            if not (isLess lowerOp lower version) then
                LT

            else if not (isLess upperOp version upper) then
                GT

            else
                EQ



-- INTERSECT


intersect : Constraint -> Constraint -> Maybe Constraint
intersect (Range lo lop hop hi) (Range lo_ lop_ hop_ hi_) =
    let
        ( newLo, newLop ) =
            case V.compare lo lo_ of
                LT ->
                    ( lo_, lop_ )

                EQ ->
                    ( lo
                    , if List.member Less [ lop, lop_ ] then
                        Less

                      else
                        LessOrEqual
                    )

                GT ->
                    ( lo, lop )

        ( newHi, newHop ) =
            case V.compare hi hi_ of
                LT ->
                    ( hi, hop )

                EQ ->
                    ( hi
                    , if List.member Less [ hop, hop_ ] then
                        Less

                      else
                        LessOrEqual
                    )

                GT ->
                    ( hi_, hop_ )
    in
    if V.compare newLo newHi /= GT then
        Just (Range newLo newLop newHop newHi)

    else
        Nothing



-- ELM CONSTRAINT


goodElm : Constraint -> Bool
goodElm constraint =
    satisfies constraint V.compiler


defaultElm : Constraint
defaultElm =
    let
        (V.Version major _ _) =
            V.compiler
    in
    if major > 0 then
        untilNextMajor V.compiler

    else
        untilNextMinor V.compiler



-- CREATE CONSTRAINTS


untilNextMajor : V.Version -> Constraint
untilNextMajor version =
    Range version LessOrEqual Less (V.bumpMajor version)


untilNextMinor : V.Version -> Constraint
untilNextMinor version =
    Range version LessOrEqual Less (V.bumpMinor version)


expand : Constraint -> V.Version -> Constraint
expand ((Range lower lowerOp upperOp upper) as constraint) version =
    if V.compare version lower == LT then
        Range version LessOrEqual upperOp upper

    else if V.compare version upper == GT then
        Range lower lowerOp Less (V.bumpMajor version)

    else
        constraint



-- JSON


encode : Constraint -> Value
encode constraint =
    E.string (toChars constraint)


decoder : Decoder Error Constraint
decoder =
    D.customString parser BadFormat



-- PARSER


type Error
    = BadFormat Row Col
    | InvalidRange V.Version V.Version


parser : P.Parser Error Constraint
parser =
    parseVersion
        |> P.bind
            (\lower ->
                P.word1 ' ' BadFormat
                    |> P.bind
                        (\_ ->
                            parseOp
                                |> P.bind
                                    (\loOp ->
                                        P.word1 ' ' BadFormat
                                            |> P.bind
                                                (\_ ->
                                                    P.word1 'v' BadFormat
                                                        |> P.bind
                                                            (\_ ->
                                                                P.word1 ' ' BadFormat
                                                                    |> P.bind
                                                                        (\_ ->
                                                                            parseOp
                                                                                |> P.bind
                                                                                    (\hiOp ->
                                                                                        P.word1 ' ' BadFormat
                                                                                            |> P.bind
                                                                                                (\_ ->
                                                                                                    parseVersion
                                                                                                        |> P.bind
                                                                                                            (\higher ->
                                                                                                                P.Parser <|
                                                                                                                    \((P.State _ _ _ _ row col) as state) ->
                                                                                                                        if V.compare lower higher == LT then
                                                                                                                            Ok (P.POk P.Empty (Range lower loOp hiOp higher) state)

                                                                                                                        else
                                                                                                                            Err (P.PErr P.Empty row col (\_ _ -> InvalidRange lower higher))
                                                                                                            )
                                                                                                )
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


parseVersion : P.Parser Error V.Version
parseVersion =
    P.specialize (\( r, c ) _ _ -> BadFormat r c) V.parser


parseOp : P.Parser Error Op
parseOp =
    P.word1 '<' BadFormat
        |> P.bind
            (\_ ->
                P.oneOfWithFallback
                    [ P.word1 '=' BadFormat
                        |> P.fmap (\_ -> LessOrEqual)
                    ]
                    Less
            )

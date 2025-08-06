module Compiler.Parse.Symbol exposing
    ( BadOperator(..)
    , badOperatorDecoder
    , badOperatorEncoder
    , binopCharSet
    , operator
    )

import Compiler.Data.Name exposing (Name)
import Compiler.Parse.NewPrimitives as P
import Data.Set as Set
import Parser exposing (..)
import Parser.Advanced as Advanced
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE



-- OPERATOR


type BadOperator
    = BadDot
    | BadPipe
    | BadArrow
    | BadEquals
    | BadHasType


operator : (P.Row -> P.Col -> x) -> (BadOperator -> P.Row -> P.Col -> x) -> P.Parser x Name
operator toExpectation toError =
    let
        op =
            getChompedString (chompWhile isBinopCharHelp)
    in
    op
        |> andThen
            (\opString ->
                case opString of
                    "" ->
                        problem ()

                    -- Mapped to toExpectation by mapError
                    "." ->
                        Advanced.problem (toError BadDot)

                    "|" ->
                        Advanced.problem (toError BadPipe)

                    "->" ->
                        Advanced.problem (toError BadArrow)

                    "=" ->
                        Advanced.problem (toError BadEquals)

                    ":" ->
                        Advanced.problem (toError BadHasType)

                    _ ->
                        succeed opString
            )
        |> Advanced.mapError (\r c _ -> toExpectation r c)


isBinopCharHelp : Char -> Bool
isBinopCharHelp char =
    Set.member char binopCharSet


binopCharSet : Set.Set Char
binopCharSet =
    Set.fromList (String.toList "+-/*=.<>:&|^?%!")



-- ENCODERS and DECODERS


badOperatorEncoder : BadOperator -> BE.Encoder
badOperatorEncoder badOperator =
    BE.unsignedInt8
        (case badOperator of
            BadDot ->
                0

            BadPipe ->
                1

            BadArrow ->
                2

            BadEquals ->
                3

            BadHasType ->
                4
        )


badOperatorDecoder : BD.Decoder BadOperator
badOperatorDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed BadDot

                    1 ->
                        BD.succeed BadPipe

                    2 ->
                        BD.succeed BadArrow

                    3 ->
                        BD.succeed BadEquals

                    4 ->
                        BD.succeed BadHasType

                    _ ->
                        BD.fail
            )

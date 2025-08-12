module Compiler.Parse.Symbol exposing
    ( BadOperator(..)
    , badOperatorDecoder
    , badOperatorEncoder
    , binopCharSet
    , operator
    )

import Compiler.Data.Name exposing (Name)
import Compiler.Parse.NewPrimitives as P
import Data.Set as EverySet exposing (EverySet)
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE



-- OPERATOR


type BadOperator
    = BadDot
    | BadPipe
    | BadArrow
    | BadEquals
    | BadHasType


operator : P.Problem -> (BadOperator -> P.Problem) -> P.Parser Name
operator expecting toProblem =
    P.getChompedString (P.chompWhile isBinopCharHelp)
        |> P.andThen
            (\op ->
                if String.isEmpty op then
                    P.problem expecting

                else
                    case op of
                        "." ->
                            P.problem (toProblem BadDot)

                        "|" ->
                            P.problem (toProblem BadPipe)

                        "->" ->
                            P.problem (toProblem BadArrow)

                        "=" ->
                            P.problem (toProblem BadEquals)

                        ":" ->
                            P.problem (toProblem BadHasType)

                        _ ->
                            P.succeed op
            )


chompOps : String -> Int -> Int -> Int
chompOps src pos end =
    if pos < end && isBinopCharHelp (P.unsafeIndex src pos) then
        chompOps src (pos + 1) end

    else
        pos


isBinopCharHelp : Char -> Bool
isBinopCharHelp char =
    let
        code : Int
        code =
            Char.toCode char
    in
    EverySet.member identity code binopCharSet


binopCharSet : EverySet Int Int
binopCharSet =
    EverySet.fromList identity (List.map Char.toCode (String.toList "+-/*=.<>:&|^?%!"))



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

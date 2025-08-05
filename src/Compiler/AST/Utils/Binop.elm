module Compiler.AST.Utils.Binop exposing
    ( Associativity(..)
    , Precedence
    , associativityDecoder
    , associativityEncoder
    , precedenceDecoder
    , precedenceEncoder
    )

import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE



-- BINOP STUFF


type alias Precedence =
    Int


type Associativity
    = Left
    | Non
    | Right



-- JSON ENCODERS and DECODERS
-- ENCODERS and DECODERS


precedenceEncoder : Precedence -> BE.Encoder
precedenceEncoder =
    BE.int


precedenceDecoder : BD.Decoder Precedence
precedenceDecoder =
    BD.int


associativityEncoder : Associativity -> BE.Encoder
associativityEncoder associativity =
    BE.unsignedInt8
        (case associativity of
            Left ->
                0

            Non ->
                1

            Right ->
                2
        )


associativityDecoder : BD.Decoder Associativity
associativityDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed Left

                    1 ->
                        BD.succeed Non

                    2 ->
                        BD.succeed Right

                    _ ->
                        BD.fail
            )

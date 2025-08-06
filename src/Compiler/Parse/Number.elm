module Compiler.Parse.Number exposing
    ( Number(..)
    , number
    , precedence
    )

import Compiler.AST.Utils.Binop as Binop
import Compiler.Parse.NewPrimitives as P
import Compiler.Reporting.Error.Syntax as E
import Parser exposing (..)
import Parser.Advanced as Advanced



-- NUMBER


type Number
    = Int Int
    | Float Float


{-| This parser has been rewritten to use `elm/parser`. The original parser
provided more detailed error messages for malformed numbers. For example, it
could distinguish between a number ending unexpectedly and a number with a
misplaced dot. This implementation uses `Parser.Advanced.number` which is less
flexible in its error reporting. As a result, some of the specific
number-related parse errors have been generalized.
-}
number : (P.Row -> P.Col -> x) -> (E.Number -> P.Row -> P.Col -> x) -> P.Parser x Number
number toExpectation toError =
    let
        numberParser =
            Advanced.number
                { int = Just Int
                , hex = Just Int
                , octal = Nothing
                , binary = Nothing
                , float = Just Float
                , invalid = Just (\_ -> E.NumberEnd)
                }
    in
    Advanced.mapError toError numberParser



-- PRECEDENCE


precedence : (P.Row -> P.Col -> x) -> P.Parser x Binop.Precedence
precedence toExpectation =
    let
        digit =
            oneOf
                [ chompChar '0' |> andThen (\_ -> succeed 0)
                , chompChar '1' |> andThen (\_ -> succeed 1)
                , chompChar '2' |> andThen (\_ -> succeed 2)
                , chompChar '3' |> andThen (\_ -> succeed 3)
                , chompChar '4' |> andThen (\_ -> succeed 4)
                , chompChar '5' |> andThen (\_ -> succeed 5)
                , chompChar '6' |> andThen (\_ -> succeed 6)
                , chompChar '7' |> andThen (\_ -> succeed 7)
                , chompChar '8' |> andThen (\_ -> succeed 8)
                , chompChar '9' |> andThen (\_ -> succeed 9)
                ]
    in
    Advanced.mapError (\r c _ -> toExpectation r c) digit

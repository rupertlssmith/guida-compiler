module Compiler.Parse.NewPrimitives exposing
    ( Col
    , Parser
    , Position
    , Row
    , andThen
    , getIndent
    , getPosition
    , located
    , map
    , oneOf
    , oneOfWithFallback
    , problem
    , succeed
    , withBacksetIndent
    , withIndent
    )

import Compiler.Reporting.Annotation as A
import Parser exposing (..)
import Parser.Advanced as Advanced


type alias Parser x a =
    Parser.Parser x a


type alias Row =
    Int


type alias Col =
    Int


type alias Position =
    A.Position


succeed : a -> Parser x a
succeed =
    Parser.succeed


problem : x -> Parser x a
problem =
    Parser.problem


map : (a -> b) -> Parser x a -> Parser x b
map =
    Parser.map


andThen : (a -> Parser x b) -> Parser x a -> Parser x b
andThen =
    Parser.andThen


oneOf : List (Parser x a) -> Parser x a
oneOf =
    Parser.oneOf


oneOfWithFallback : List (Parser x a) -> a -> Parser x a
oneOfWithFallback parsers fallback =
    oneOf (parsers ++ [ succeed fallback ])


getPosition : Parser x Position
getPosition =
    Parser.map2 A.Position Parser.getRow Parser.getCol


located : Parser x a -> Parser x (A.Located a)
located p =
    succeed (\start v end -> A.at start end v)
        |> andMap getPosition
        |> andMap p
        |> andMap getPosition


getIndent : Parser x Int
getIndent =
    Parser.getIndent


withIndent : Parser x a -> Parser x a
withIndent =
    Parser.withIndent


withBacksetIndent : Int -> Parser x a -> Parser x a
withBacksetIndent backset p =
    getCol
        |> andThen
            (\col ->
                Advanced.withIndent (col - backset) p
            )

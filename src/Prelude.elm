module Prelude exposing
    ( head
    , init
    , last
    , putStrLn
    )

import Data.IO as IO exposing (IO)
import Json.Decode as Decode
import List.Extra as List
import Utils.Crash exposing (crash)


head : List a -> a
head items =
    case List.head items of
        Just item ->
            item

        Nothing ->
            crash "*** Exception: Prelude.head: empty list"


init : List a -> List a
init items =
    case List.init items of
        Just initItems ->
            initItems

        Nothing ->
            crash "*** Exception: Prelude.init: empty list"


last : List a -> a
last items =
    case List.last items of
        Just item ->
            item

        Nothing ->
            crash "*** Exception: Prelude.last: empty list"


putStrLn : String -> IO ()
putStrLn s =
    IO.make (Decode.succeed ()) (IO.PutStrLn s)

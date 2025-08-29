module Utils.Crash exposing (crash)

import Json.Encode as Encode


crash : String -> a
crash str =
    Encode.object [ ( "__guida_crash", Encode.list identity [ Encode.string str ] ) ]
        |> always (crash str)

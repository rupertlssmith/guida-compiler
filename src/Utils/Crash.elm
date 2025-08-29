module Utils.Crash exposing (crash)

import Json.Encode as Encode


crash : String -> a
crash str =
    let
        _ =
            Encode.object [ ( "__guida_crash", Encode.list identity [ Encode.string str ] ) ]
    in
    crash str

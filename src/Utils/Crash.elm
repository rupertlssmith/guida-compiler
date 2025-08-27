module Utils.Crash exposing (crash)

import Json.Encode as E


crash : String -> a
crash str =
    let
        _ =
            E.object [ ( "__elm_crash", E.list identity [ E.string str ] ) ]
    in
    crash str

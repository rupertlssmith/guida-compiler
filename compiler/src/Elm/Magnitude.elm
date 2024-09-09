module Elm.Magnitude exposing
    ( Magnitude(..)
    , toChars
    , toString
    )

-- MAGNITUDE


type Magnitude
    = PATCH
    | MINOR
    | MAJOR


toChars : Magnitude -> String
toChars magnitude =
    case magnitude of
        PATCH ->
            "PATCH"

        MINOR ->
            "MINOR"

        MAJOR ->
            "MAJOR"


toString : Magnitude -> String
toString =
    toChars

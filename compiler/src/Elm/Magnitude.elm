module Elm.Magnitude exposing
    ( Magnitude(..)
    , compare
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


compare : Magnitude -> Magnitude -> Order
compare m1 m2 =
    let
        toInt m =
            case m of
                PATCH ->
                    0

                MINOR ->
                    1

                MAJOR ->
                    2
    in
    Basics.compare (toInt m1) (toInt m2)

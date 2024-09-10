module Hello exposing (main)

import Html exposing (text)


fn1 : { bar : String, foo : String } -> String
fn1 { bar, foo } =
    bar ++ foo 


main =
    text (fn1 { bar = "lo!", foo = "Hel" })

module Try exposing (..)

f x =
    let
        g : (a -> ()) -> ()
        g h =
            h x
    in
    x


main : Platform.Program () () ()
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }

module Node.Main exposing (main)

import Json.Decode as Decode
import Json.Encode as Encode
import Node.Format as Format
import System.IO as IO exposing (IO)
import Utils.Impure as Impure


main : IO.Program
main =
    IO.run app


app : IO ()
app =
    getArgs
        |> IO.bind
            (\args ->
                case args of
                    FormatArgs path ->
                        case Format.run path of
                            Ok output ->
                                exitWithResponse (Encode.object [ ( "output", Encode.string output ) ])

                            Err error ->
                                exitWithResponse (Encode.object [ ( "error", Encode.string error ) ])
            )


getArgs : IO Args
getArgs =
    Impure.task "getArgs" [] Impure.EmptyBody (Impure.DecoderResolver argsDecoder)


exitWithResponse : Encode.Value -> IO a
exitWithResponse value =
    Impure.task "exitWithResponse" [] (Impure.JsonBody value) Impure.Crash



-- ARGS


type Args
    = FormatArgs String


argsDecoder : Decode.Decoder Args
argsDecoder =
    Decode.field "command" Decode.string
        |> Decode.andThen
            (\command ->
                case command of
                    "format" ->
                        Decode.map FormatArgs
                            (Decode.field "content" Decode.string)

                    _ ->
                        Decode.fail ("Unknown command: " ++ command)
            )

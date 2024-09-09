port module Main exposing (main)

-- import Bump
-- import Develop
-- import Diff
-- import Publish
-- import Repl

import Array exposing (Array)
import Array.Extra as Array
import Data.IO as IO exposing (IO(..))
import Elm.Package as Pkg
import Elm.Version as V
import Init
import Install
import Json.Decode as Decode
import Json.Encode as Encode
import Make
import Parse.Primitives as P
import Reporting.Doc as D
import Task
import Terminal
import Terminal.Helpers as Terminal
import Terminal.Internal as Terminal


type alias Flags =
    Decode.Value


type Command
    = Init
      -- | Repl Repl.Flags
    | Make (List String) Make.Flags
    | Install Install.Args


commandDecoder : Decode.Decoder Command
commandDecoder =
    Decode.field "command" Decode.string
        |> Decode.andThen
            (\command ->
                case command of
                    "init" ->
                        Decode.succeed Init

                    -- "repl" ->
                    --     Decode.map Repl
                    --         (Decode.map2 Repl.Flags
                    --             (Decode.maybe (Decode.field "maybeAlternateInterpreter" Decode.string))
                    --             (Decode.map (Maybe.withDefault False) (Decode.maybe (Decode.field "noColors" Decode.bool)))
                    --         )
                    "make" ->
                        Decode.map2 Make
                            (Decode.field "paths" (Decode.list Decode.string))
                            (Decode.map5 Make.Flags
                                (Decode.map (Maybe.withDefault False) (Decode.maybe (Decode.field "debug" Decode.bool)))
                                (Decode.map (Maybe.withDefault False) (Decode.maybe (Decode.field "optimize" Decode.bool)))
                                (Decode.map (Maybe.andThen Make.parseOutput) (Decode.maybe (Decode.field "output" Decode.string)))
                                (Decode.map (Maybe.andThen Make.parseReportType) (Decode.maybe (Decode.field "report" Decode.string)))
                                (Decode.maybe (Decode.field "docs" Decode.string))
                            )

                    "install" ->
                        Decode.andThen
                            (\maybePackage ->
                                case maybePackage of
                                    Just package ->
                                        case P.fromByteString Pkg.parser Tuple.pair package of
                                            Ok packageName ->
                                                Decode.succeed (Install (Install.Install packageName))

                                            Err _ ->
                                                Decode.fail ("Invalid package name: " ++ package)

                                    Nothing ->
                                        Decode.succeed (Install Install.NoArgs)
                            )
                            (Decode.maybe (Decode.field "package" Decode.string))

                    _ ->
                        Decode.fail ("Unknown command: " ++ command)
            )



-- PROGRAM


type alias PortIn =
    ({ index : Int, value : Encode.Value } -> Msg) -> Sub Msg


type alias PortOut =
    { index : Int, value : Encode.Value } -> Cmd Msg


type alias Model =
    Array ProcessStatus


type ProcessStatus
    = Running IO.Process
    | Finished


type Msg
    = Msg Int Encode.Value


{-| -}
program : PortIn -> PortOut -> Program Flags Model Msg
program portIn portOut =
    Platform.worker
        { init =
            \flags ->
                case Decode.decodeValue commandDecoder flags of
                    Ok command ->
                        let
                            ( process, effect, _ ) =
                                start
                                    (case command of
                                        Init ->
                                            Init.run

                                        -- Repl replFlags ->
                                        --     Repl.run replFlags
                                        Make paths makeFlags ->
                                            Make.run paths makeFlags

                                        Install args ->
                                            Install.run args
                                    )
                        in
                        ( Array.fromList [ Running process ]
                        , effectToCmd 0 portOut effect
                        )

                    Err err ->
                        ( Array.empty, effectToCmd 0 portOut (IO.Exit (Decode.errorToString err) 1) )
        , update =
            \msg model ->
                case msg of
                    Msg index value ->
                        case Array.get index model of
                            Just (Running process) ->
                                case step value process of
                                    Ok ( nextProcess, effect, maybeFork ) ->
                                        ( Array.set index (Running nextProcess) model
                                        , effectToCmd index portOut effect
                                        )
                                            |> addFork portOut maybeFork

                                    Err (Decode.Failure "exitFork" _) ->
                                        ( Array.set index Finished model, Cmd.none )

                                    Err err ->
                                        ( model
                                        , effectToCmd index portOut (IO.Exit (Decode.errorToString err) 255)
                                        )

                            Just Finished ->
                                ( model
                                , effectToCmd index portOut (IO.Exit ("Process has already finished! Index: " ++ String.fromInt index) 255)
                                )

                            Nothing ->
                                ( model
                                , effectToCmd index portOut (IO.Exit ("Could not find process! Index: " ++ String.fromInt index) 255)
                                )
        , subscriptions = \_ -> portIn (\{ index, value } -> Msg index value)
        }


addFork : PortOut -> Maybe (IO ()) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addFork portOut maybeFork ( model, cmd ) =
    case Maybe.map startFork maybeFork of
        Just ( process, effect, _ ) ->
            let
                nextIndex =
                    Array.length model
            in
            ( Array.insertAt nextIndex (Running process) model
            , Cmd.batch [ effectToCmd nextIndex portOut effect, cmd ]
            )

        Nothing ->
            ( model, cmd )


port send : { index : Int, value : Encode.Value } -> Cmd msg


port recv : ({ index : Int, value : Encode.Value } -> msg) -> Sub msg


main : Program Flags Model Msg
main =
    program recv send


start : IO () -> ( IO.Process, IO.Effect, Maybe (IO ()) )
start (IO io) =
    io (\_ -> ( IO.Process (Decode.fail "Exit"), IO.Exit "finished successfully..." 0, Nothing ))


startFork : IO () -> ( IO.Process, IO.Effect, Maybe (IO ()) )
startFork (IO io) =
    io (\_ -> ( IO.Process (Decode.fail "exitFork"), IO.NoOp, Nothing ))


effectToCmd : Int -> PortOut -> IO.Effect -> Cmd Msg
effectToCmd index portOut effect =
    case effect of
        IO.Exit errorMessage status ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "exit" )
                        , ( "args", Encode.list identity [ Encode.string errorMessage, Encode.int status ] )
                        ]
                }

        IO.NewIORef value ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "newIORef" )
                        , ( "args", Encode.list identity [ value ] )
                        ]
                }

        IO.ReadIORef id ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "readIORef" )
                        , ( "args", Encode.list Encode.int [ id ] )
                        ]
                }

        IO.WriteIORef id value ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "writeIORef" )
                        , ( "args"
                          , Encode.list identity
                                [ Encode.int id
                                , value
                                ]
                          )
                        ]
                }

        IO.GetLine ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "getLine" )
                        , ( "args", Encode.list identity [] )
                        ]
                }

        IO.HPutStr handle content ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "fwrite" )
                        , ( "args"
                          , Encode.list identity
                                [ Encode.int
                                    (case handle of
                                        IO.Stdout ->
                                            1

                                        IO.Stderr ->
                                            2
                                    )
                                , Encode.string content
                                ]
                          )
                        ]
                }

        IO.Write path value ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "write" )
                        , ( "args", Encode.list identity [ Encode.string path, value ] )
                        ]
                }

        IO.WriteString path str ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "writeString" )
                        , ( "args", Encode.list Encode.string [ path, str ] )
                        ]
                }

        IO.PutStrLn str ->
            effectToCmd index portOut (IO.HPutStr IO.Stdout (str ++ "\n"))

        IO.DirDoesFileExist filename ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirDoesFileExist" )
                        , ( "args", Encode.list Encode.string [ filename ] )
                        ]
                }

        IO.DirDoesDirectoryExist path ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirDoesDirectoryExist" )
                        , ( "args", Encode.list Encode.string [ path ] )
                        ]
                }

        IO.EnvLookupEnv varname ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "envLookupEnv" )
                        , ( "args", Encode.list Encode.string [ varname ] )
                        ]
                }

        IO.BinaryDecodeFileOrFail filename ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "binaryDecodeFileOrFail" )
                        , ( "args", Encode.list Encode.string [ filename ] )
                        ]
                }

        IO.DirCreateDirectoryIfMissing createParents filename ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirCreateDirectoryIfMissing" )
                        , ( "args"
                          , Encode.list identity
                                [ Encode.bool createParents
                                , Encode.string filename
                                ]
                          )
                        ]
                }

        IO.Read path ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "fread" )
                        , ( "args", Encode.list Encode.string [ path ] )
                        ]
                }

        IO.HttpFetch method url ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "httpFetch" )
                        , ( "args", Encode.list Encode.string [ method, url ] )
                        ]
                }

        IO.DirGetAppUserDataDirectory app ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirGetAppUserDataDirectory" )
                        , ( "args", Encode.list Encode.string [ app ] )
                        ]
                }

        IO.DirGetCurrentDirectory ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirGetCurrentDirectory" )
                        , ( "args", Encode.list identity [] )
                        ]
                }

        IO.DirGetModificationTime filename ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirGetModificationTime" )
                        , ( "args", Encode.list Encode.string [ filename ] )
                        ]
                }

        IO.DirCanonicalizePath path ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "dirCanonicalizePath" )
                        , ( "args", Encode.list Encode.string [ path ] )
                        ]
                }

        IO.GetArchive method url ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "getArchive" )
                        , ( "args", Encode.list Encode.string [ method, url ] )
                        ]
                }

        IO.LockFile path ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "lockFile" )
                        , ( "args", Encode.list Encode.string [ path ] )
                        ]
                }

        IO.UnlockFile path ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "unlockFile" )
                        , ( "args", Encode.list Encode.string [ path ] )
                        ]
                }

        IO.NewEmptyMVar ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "newEmptyMVar" )
                        , ( "args", Encode.list identity [] )
                        ]
                }

        IO.ReadMVar id ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "readMVar" )
                        , ( "args", Encode.list Encode.int [ id ] )
                        ]
                }

        IO.TakeMVar id ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "takeMVar" )
                        , ( "args", Encode.list Encode.int [ id ] )
                        ]
                }

        IO.PutMVar id value ->
            portOut
                { index = index
                , value =
                    Encode.object
                        [ ( "fn", Encode.string "putMVar" )
                        , ( "args"
                          , Encode.list identity
                                [ Encode.int id
                                , value
                                ]
                          )
                        ]
                }

        IO.NoOp ->
            Task.succeed Encode.null
                |> Task.perform (Msg index)

        notImplementedEffect ->
            effectToCmd index portOut (IO.Exit ("Effect not implemented: " ++ Debug.toString notImplementedEffect) 254)


step : Encode.Value -> IO.Process -> Result Decode.Error ( IO.Process, IO.Effect, Maybe (IO ()) )
step value (IO.Process decoder) =
    Decode.decodeValue decoder value



-- MAIN


main_ : IO ()
main_ =
    Terminal.app intro
        outro
        [ -- repl
          init

        -- , reactor
        -- , make
        -- , install
        -- , bump
        -- , diff
        -- , publish
        ]


intro : D.Doc
intro =
    D.vcat
        [ D.fillSep
            [ D.fromChars "Hi,"
            , D.fromChars "thank"
            , D.fromChars "you"
            , D.fromChars "for"
            , D.fromChars "trying"
            , D.fromChars "out"
            , D.green (D.fromChars "Elm")
            , D.green (D.fromChars (V.toChars V.compiler))
                |> D.a (D.fromChars ".")
            , D.fromChars "I hope you like it!"
            ]
        , D.fromChars ""
        , D.black (D.fromChars "-------------------------------------------------------------------------------")
        , D.black (D.fromChars "I highly recommend working through <https://guide.elm-lang.org> to get started.")
        , D.black (D.fromChars "It teaches many important concepts, including how to use `elm` in the terminal.")
        , D.black (D.fromChars "-------------------------------------------------------------------------------")
        ]


outro : D.Doc
outro =
    D.fillSep <|
        (List.map D.fromChars <|
            String.words <|
                "Be sure to ask on the Elm slack if you run into trouble! Folks are friendly and happy to help out. They hang out there because it is fun, so be kind to get the best results!"
        )



-- INIT


init : Terminal.Command
init =
    let
        summary =
            "Start an Elm project. It creates a starter elm.json file and provides a link explaining what to do from there."

        details =
            "The `init` command helps start Elm projects:"

        example =
            reflow
                "It will ask permission to create an elm.json file, the one thing common to all Elm projects. It also provides a link explaining what to do from there."
    in
    Terminal.Command "init" (Terminal.Common summary) details example (Terminal.ArgDocs (\_ -> [])) (Terminal.FlagDocs []) <|
        \_ ->
            Ok Init.run



-- REPL
-- repl : Terminal.Command
-- repl =
--     let
--         summary =
--             "Open up an interactive programming session. Type in Elm expressions like (2 + 2) or (String.length \"test\") and see if they equal four!"
--         details =
--             "The `repl` command opens up an interactive programming session:"
--         example =
--             reflow
--                 "Start working through <https://guide.elm-lang.org> to learn how to use this! It has a whole chapter that uses the REPL for everything, so that is probably the quickest way to get started."
--         replFlags =
--             Terminal.flags Repl.Flags
--                 |> Terminal.more (Terminal.flag "interpreter" interpreter "Path to a alternate JS interpreter, like node or nodejs.")
--                 |> Terminal.more (Terminal.onOff "no-colors" "Turn off the colors in the REPL. This can help if you are having trouble reading the values. Some terminals use a custom color scheme that diverges significantly from the standard ANSI colors, so another path may be to pick a more standard color scheme.")
--     in
--     -- TODO
--     Terminal.Command "repl" (Terminal.Common summary) details example (Terminal.ArgDocs (\_ -> [])) (Terminal.FlagDocs []) <|
--         \_ ->
--             Ok (Repl.run () (Repl.Flags Nothing False))
-- interpreter : Terminal.Parser String
-- interpreter =
--     Terminal.Parser
--         "interpreter"
--         "interpreters"
--         Just
--         (\_ -> IO.pure [])
--         (\_ -> IO.pure [ "node", "nodejs" ])
-- REACTOR
-- reactor : Terminal.Command
-- reactor =
--     let
--         summary =
--             "Compile code with a click. It opens a file viewer in your browser, and when you click on an Elm file, it compiles and you see the result."
--         details =
--             "The `reactor` command starts a local server on your computer:"
--         example =
--             reflow
--                 "After running that command, you would have a server at <http://localhost:8000> that helps with development. It shows your files like a file viewer. If you click on an Elm file, it will compile it for you! And you can just press the refresh button in the browser to recompile things."
--     in
--     Terminal.Command "reactor" (Terminal.Common summary) details example <|
--         \_ ->
--             -- TODO
--             Develop.run () (Develop.Flags Nothing)
-- port_ : Terminal.Parser Int
-- port_ =
--     Terminal.Parser
--         "port"
--         "ports"
--         String.toInt
--         (\_ -> IO.pure [])
--         (\_ -> IO.pure [ "3000", "8000" ])
-- MAKE
-- make : Terminal.Command
-- make =
--     let
--         details =
--             "The `make` command compiles Elm code into JS or HTML:"
--         example =
--             stack
--                 [ reflow "For example:"
--                 , D.indent 4 <| D.green (D.fromChars "elm make src/Main.elm")
--                 , reflow "This tries to compile an Elm file named src/Main.elm, generating an index.html file if possible."
--                 ]
--     in
--     -- TODO
--     Terminal.Command "make" Terminal.Uncommon details example (Terminal.ArgDocs (\_ -> [])) (Terminal.FlagDocs []) <|
--         \_ ->
--             Ok (Make.run [] (Make.Flags False False Nothing Nothing Nothing))
-- INSTALL
-- install : Terminal.Command
-- install =
--     let
--         details =
--             "The `install` command fetches packages from <https://package.elm-lang.org> for use in your project:"
--         example =
--             stack
--                 [ reflow
--                     "For example, if you want to get packages for HTTP and JSON, you would say:"
--                 , D.indent 4 <|
--                     D.green <|
--                         D.vcat <|
--                             [ D.fromChars "elm install elm/http"
--                             , D.fromChars "elm install elm/json"
--                             ]
--                 , reflow
--                     "Notice that you must say the AUTHOR name and PROJECT name! After running those commands, you could say `import Http` or `import Json.Decode` in your code."
--                 , reflow
--                     "What if two projects use different versions of the same package? No problem! Each project is independent, so there cannot be conflicts like that!"
--                 ]
--     in
--     -- TODO
--     Terminal.Command "install" Terminal.Uncommon details example (Terminal.ArgDocs (\_ -> [])) (Terminal.FlagDocs []) <|
--         \_ ->
--             Ok (Install.run Install.NoArgs ())
-- PUBLISH
-- publish : Terminal.Command
-- publish =
--     let
--         details =
--             "The `publish` command publishes your package on <https://package.elm-lang.org> so that anyone in the Elm community can use it."
--         example =
--             stack
--                 [ reflow
--                     "Think hard if you are ready to publish NEW packages though!"
--                 , reflow
--                     "Part of what makes Elm great is the packages ecosystem. The fact that there is usually one option (usually very well done) makes it way easier to pick packages and become productive. So having a million packages would be a failure in Elm. We do not need twenty of everything, all coded in a single weekend."
--                 , reflow
--                     "So as community members gain wisdom through experience, we want them to share that through thoughtful API design and excellent documentation. It is more about sharing ideas and insights than just sharing code! The first step may be asking for advice from people you respect, or in community forums. The second step may be using it at work to see if it is as nice as you think. Maybe it ends up as an experiment on GitHub only. Point is, try to be respectful of the community and package ecosystem!"
--                 , reflow
--                     "Check out <https://package.elm-lang.org/help/design-guidelines> for guidance on how to create great packages!"
--                 ]
--     in
--     Terminal.Command "publish" Terminal.Uncommon details example (\_ -> Publish.run () ())
-- BUMP
-- bump : Terminal.Command
-- bump =
--     let
--         details =
--             "The `bump` command figures out the next version number based on API changes:"
--         example =
--             reflow
--                 "Say you just published version 1.0.0, but then decided to remove a function. I will compare the published API to what you have locally, figure out that it is a MAJOR change, and bump your version number to 2.0.0. I do this with all packages, so there cannot be MAJOR changes hiding in PATCH releases in Elm!"
--     in
--     Terminal.Command "bump" Terminal.Uncommon details example (\_ -> Bump.run () ())
-- DIFF
-- diff : Terminal.Command
-- diff =
--     let
--         details =
--             "The `diff` command detects API changes:"
--         example =
--             stack
--                 [ reflow
--                     "For example, to see what changed in the HTML package between versions 1.0.0 and 2.0.0, you can say:"
--                 , D.indent 4 <| D.green <| D.fromChars "elm diff elm/html 1.0.0 2.0.0"
--                 , reflow
--                     "Sometimes a MAJOR change is not actually very big, so this can help you plan your upgrade timelines."
--                 ]
--     in
--     Terminal.Command "diff" Terminal.Uncommon details example <|
--         \_ ->
--             Diff.run Diff.CodeVsLatest ()
-- HELPERS


stack : List D.Doc -> D.Doc
stack docs =
    D.vcat <| List.intersperse (D.fromChars "") docs


reflow : String -> D.Doc
reflow string =
    D.fillSep <| List.map D.fromChars <| String.words string

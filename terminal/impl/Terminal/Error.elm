module Terminal.Error exposing
    ( exitWithError
    , exitWithHelp
    , exitWithOverview
    , exitWithUnknown
    )

import Data.IO as IO exposing (IO)
import Prelude
import Reporting.Doc as D
import Reporting.Suggest as Suggest
import Terminal.Internal
    exposing
        ( ArgDocs(..)
        , ArgError(..)
        , Args(..)
        , Command(..)
        , CompleteArgs(..)
        , CompleteArgsValue(..)
        , Error(..)
        , Expectation(..)
        , Flag(..)
        , FlagDocs(..)
        , FlagError(..)
        , Flags(..)
        , Parser(..)
        , RequiredArgs(..)
        , Summary(..)
        , toName
        )
import Utils.Main as Utils



-- EXIT


exitSuccess : List D.Doc -> IO a
exitSuccess =
    exitWith IO.ExitSuccess


exitFailure : List D.Doc -> IO a
exitFailure =
    exitWith (IO.ExitFailure 1)


exitWith : IO.ExitCode -> List D.Doc -> IO a
exitWith code docs =
    -- IO.hIsTerminalDevice IO.stderr
    --     |> IO.bind
    --         (\isTerminal ->
    --             let
    --                 adjust : D.Doc -> D.Doc
    --                 adjust =
    --                     if isTerminal then
    --                         identity
    --                     else
    --                         -- D.plain
    --                         Debug.todo "exitWith"
    --             in
    --             D.displayIO IO.stderr
    --                 (D.renderPretty 1
    --                     80
    --                     (adjust (D.vcat (List.concatMap (\d -> [ d, "" ]) docs)))
    --                     |> IO.bind (\_ -> IO.hPutStrLn IO.stderr "")
    --                     |> IO.bind (\_ -> IO.exitWith code)
    --                 )
    --         )
    Debug.todo "exitWith"


getExeName : IO String
getExeName =
    IO.fmap Utils.fpTakeFileName Utils.envGetProgName


stack : List D.Doc -> D.Doc
stack docs =
    D.vcat <| List.intersperse (D.fromChars "") docs


reflow : String -> D.Doc
reflow string =
    D.fillSep <| List.map D.fromChars <| String.words string



-- HELP


exitWithHelp : Maybe String -> String -> D.Doc -> ArgDocs -> FlagDocs -> IO a
exitWithHelp maybeCommand details example (ArgDocs argDocs) (FlagDocs flagDocs) =
    toCommand maybeCommand
        |> IO.bind
            (\command ->
                exitSuccess <|
                    [ reflow details
                    , D.indent 4 <| D.cyan <| D.vcat <| argDocs command
                    , example
                    ]
                        ++ (case flagDocs of
                                [] ->
                                    []

                                (_ :: _) as docs ->
                                    [ D.fromChars "You can customize this command with the following flags:"
                                    , D.indent 4 <| stack docs
                                    ]
                           )
            )


toCommand : Maybe String -> IO String
toCommand maybeCommand =
    getExeName
        |> IO.fmap
            (\exeName ->
                case maybeCommand of
                    Nothing ->
                        exeName

                    Just command ->
                        exeName ++ " " ++ command
            )


argsToDoc : String -> CompleteArgs a -> D.Doc
argsToDoc command args =
    case args of
        Exactly required ->
            argsToDocHelp command required []

        Multiple required (Parser _ plural _ _ _) ->
            argsToDocHelp command required [ "zero or more " ++ plural ]

        Optional required (Parser singular _ _ _ _) ->
            argsToDocHelp command required [ "optional " ++ singular ]


argsToDocHelp : String -> RequiredArgs a -> List String -> D.Doc
argsToDocHelp command args names =
    -- case args of
    --     Done _ ->
    --         D.hang 4 <|
    --             D.hsep <|
    --                 List.map D.fromChars <|
    --                     (command :: List.map toToken names)
    --     Required others (Parser singular _ _ _ _) ->
    --         argsToDocHelp command others (singular :: names)
    Debug.todo "argsToDocHelp"


toToken : String -> String
toToken string =
    "<"
        ++ String.map
            (\c ->
                if c == ' ' then
                    '-'

                else
                    c
            )
            string
        ++ ">"


flagsToDocs : Flags flags -> List D.Doc -> List D.Doc
flagsToDocs flags docs =
    -- case flags of
    --     FDone _ ->
    --         docs
    --     FMore more flag ->
    --         let
    --             flagDoc =
    --                 D.vcat <|
    --                     case flag of
    --                         Flag name (Parser singular _ _ _ _) description ->
    --                             [ D.dullcyan <| D.fromChars <| "--" ++ name ++ "=" ++ toToken singular
    --                             , D.indent 4 <| reflow description
    --                             ]
    --                         OnOff name description ->
    --                             [ D.dullcyan <| D.fromChars <| "--" ++ name
    --                             , D.indent 4 <| reflow description
    --                             ]
    --         in
    --         flagsToDocs more (flagDoc :: docs)
    Debug.todo "flagsToDocs"



-- OVERVIEW


exitWithOverview : D.Doc -> D.Doc -> List Command -> IO a
exitWithOverview intro outro commands =
    getExeName
        |> IO.bind
            (\exeName ->
                exitSuccess
                    [ intro
                    , D.fromChars "The most common commands are:"
                    , D.indent 4 <| stack <| List.filterMap (toSummary exeName) commands
                    , D.fromChars "There are a bunch of other commands as well though. Here is a full list:"
                    , D.indent 4 <| D.dullcyan <| toCommandList exeName commands
                    , D.fromChars "Adding the --help flag gives a bunch of additional details about each one."
                    , outro
                    ]
            )


toSummary : String -> Command -> Maybe D.Doc
toSummary exeName (Command name summary _ _ (ArgDocs argDocs) _ _) =
    case summary of
        Uncommon ->
            Nothing

        Common summaryString ->
            Just <|
                D.vcat
                    [ D.cyan <| Prelude.head (argDocs (exeName ++ " " ++ name))
                    , D.indent 4 <| reflow summaryString
                    ]


toCommandList : String -> List Command -> D.Doc
toCommandList exeName commands =
    let
        names =
            List.map toName commands

        width =
            Utils.listMaximum (List.map String.length names)

        toExample name =
            D.fromChars
                (exeName
                    ++ " "
                    ++ name
                    ++ String.repeat (width - String.length name) " "
                    ++ " --help"
                )
    in
    D.vcat (List.map toExample names)



-- UNKNOWN


exitWithUnknown : String -> List String -> IO a
exitWithUnknown unknown knowns =
    let
        nearbyKnowns =
            Utils.takeWhile (\( r, _ ) -> r <= 3) (Suggest.rank unknown identity knowns)

        suggestions =
            case List.map toGreen (List.map Tuple.second nearbyKnowns) of
                [] ->
                    []

                [ nearby ] ->
                    [ D.fromChars "Try", nearby, D.fromChars "instead?" ]

                [ a, b ] ->
                    [ D.fromChars "Try", a, D.fromChars "or", b, D.fromChars "instead?" ]

                (_ :: _ :: _ :: _) as abcs ->
                    D.fromChars "Try"
                        :: List.map (D.a (D.fromChars ",")) (Prelude.init abcs)
                        ++ [ D.fromChars "or", Prelude.last abcs, D.fromChars "instead?" ]
    in
    getExeName
        |> IO.bind
            (\exeName ->
                exitFailure
                    [ D.fillSep <|
                        [ D.fromChars "There"
                        , D.fromChars "is"
                        , D.fromChars "no"
                        , toRed unknown
                        , D.fromChars "command."
                        ]
                            ++ suggestions
                    , reflow <| "Run `" ++ exeName ++ "` with no arguments to get more hints."
                    ]
            )



-- ERROR TO DOC


exitWithError : Error -> IO a
exitWithError err =
    IO.bind exitFailure
        (case err of
            BadFlag flagError ->
                flagErrorToDocs flagError

            BadArgs argErrors ->
                case argErrors of
                    [] ->
                        IO.pure
                            [ reflow <| "I was not expecting any arguments for this command."
                            , reflow <| "Try removing them?"
                            ]

                    [ ( _, argError ) ] ->
                        argErrorToDocs argError

                    _ :: _ :: _ ->
                        argErrorToDocs <| Prelude.head <| List.sortBy toArgErrorRank (List.map Tuple.second argErrors)
        )


toArgErrorRank :
    ArgError
    -> Int -- lower is better
toArgErrorRank err =
    case err of
        ArgBad _ _ ->
            0

        ArgMissing _ ->
            1

        ArgExtras _ ->
            2


toGreen : String -> D.Doc
toGreen str =
    D.green (D.fromChars str)


toYellow : String -> D.Doc
toYellow str =
    D.yellow (D.fromChars str)


toRed : String -> D.Doc
toRed str =
    D.red (D.fromChars str)



-- ARG ERROR TO DOC


argErrorToDocs : ArgError -> IO (List D.Doc)
argErrorToDocs argError =
    case argError of
        ArgMissing (Expectation tipe makeExamples) ->
            makeExamples
                |> IO.fmap
                    (\examples ->
                        [ D.fillSep
                            [ D.fromChars "The"
                            , D.fromChars "arguments"
                            , D.fromChars "you"
                            , D.fromChars "have"
                            , D.fromChars "are"
                            , D.fromChars "fine,"
                            , D.fromChars "but"
                            , D.fromChars "in"
                            , D.fromChars "addition,"
                            , D.fromChars "I"
                            , D.fromChars "was"
                            , D.fromChars "expecting"
                            , D.fromChars "a"
                            , toYellow (toToken tipe)
                            , D.fromChars "value."
                            , D.fromChars "For"
                            , D.fromChars "example:"
                            ]
                        , D.indent 4 <| D.green <| D.vcat <| List.map D.fromChars examples
                        ]
                    )

        ArgBad string (Expectation tipe makeExamples) ->
            makeExamples
                |> IO.fmap
                    (\examples ->
                        [ D.fromChars "I am having trouble with this argument:"
                        , D.indent 4 <| toRed string
                        , D.fillSep <|
                            [ D.fromChars "It"
                            , D.fromChars "is"
                            , D.fromChars "supposed"
                            , D.fromChars "to"
                            , D.fromChars "be"
                            , D.fromChars "a"
                            , toYellow (toToken tipe)
                            , D.fromChars "value,"
                            , D.fromChars "like"
                            ]
                                ++ (if List.length examples == 1 then
                                        [ D.fromChars "this:" ]

                                    else
                                        [ D.fromChars "one"
                                        , D.fromChars "of"
                                        , D.fromChars "these:"
                                        ]
                                   )
                        , D.indent 4 <| D.green <| D.vcat <| List.map D.fromChars examples
                        ]
                    )

        ArgExtras extras ->
            let
                ( these, them ) =
                    case extras of
                        [ _ ] ->
                            ( "this argument", "it" )

                        _ ->
                            ( "these arguments", "them" )
            in
            IO.pure
                [ reflow <| "I was not expecting " ++ these ++ ":"
                , D.indent 4 <| D.red <| D.vcat <| List.map D.fromChars extras
                , reflow <| "Try removing " ++ them ++ "?"
                ]



-- FLAG ERROR TO DOC


flagErrorHelp : String -> String -> List D.Doc -> IO (List D.Doc)
flagErrorHelp summary original explanation =
    IO.pure <|
        [ reflow summary
        , D.indent 4 (toRed original)
        ]
            ++ explanation


flagErrorToDocs : FlagError -> IO (List D.Doc)
flagErrorToDocs flagError =
    case flagError of
        FlagWithValue flagName value ->
            flagErrorHelp
                "This on/off flag was given a value:"
                ("--" ++ flagName ++ "=" ++ value)
                [ D.fromChars "An on/off flag either exists or not. It cannot have an equals sign and value.\nMaybe you want this instead?"
                , D.indent 4 <| toGreen <| "--" ++ flagName
                ]

        FlagWithNoValue flagName (Expectation tipe makeExamples) ->
            makeExamples
                |> IO.bind
                    (\examples ->
                        flagErrorHelp
                            "This flag needs more information:"
                            ("--" ++ flagName)
                            [ D.fillSep
                                [ D.fromChars "It"
                                , D.fromChars "needs"
                                , D.fromChars "a"
                                , toYellow (toToken tipe)
                                , D.fromChars "like"
                                , D.fromChars "this:"
                                ]
                            , D.indent 4 <|
                                D.vcat <|
                                    List.map toGreen <|
                                        case List.take 4 examples of
                                            [] ->
                                                [ "--" ++ flagName ++ "=" ++ toToken tipe ]

                                            _ :: _ ->
                                                List.map (\example -> "--" ++ flagName ++ "=" ++ example) examples
                            ]
                    )

        FlagWithBadValue flagName badValue (Expectation tipe makeExamples) ->
            makeExamples
                |> IO.bind
                    (\examples ->
                        flagErrorHelp
                            "This flag was given a bad value:"
                            ("--" ++ flagName ++ "=" ++ badValue)
                            [ D.fillSep <|
                                [ D.fromChars "I"
                                , D.fromChars "need"
                                , D.fromChars "a"
                                , D.fromChars "valid"
                                , toYellow (toToken tipe)
                                , D.fromChars "value."
                                , D.fromChars "For"
                                , D.fromChars "example:"
                                ]
                            , D.indent 4 <|
                                D.vcat <|
                                    List.map toGreen <|
                                        case List.take 4 examples of
                                            [] ->
                                                [ "--" ++ flagName ++ "=" ++ toToken tipe ]

                                            _ :: _ ->
                                                List.map (\example -> "--" ++ flagName ++ "=" ++ example) examples
                            ]
                    )

        FlagUnknown unknown flags ->
            flagErrorHelp
                "I do not recognize this flag:"
                unknown
                (let
                    unknownName =
                        -- List.takeWhile ((/=) '=') (List.dropWhile ((==) '-') unknown)
                        Debug.todo "unknownName"
                 in
                 case getNearbyFlags unknownName flags [] of
                    [] ->
                        []

                    [ thisOne ] ->
                        [ D.fillSep
                            [ D.fromChars "Maybe"
                            , D.fromChars "you"
                            , D.fromChars "want"
                            , D.green thisOne
                            , D.fromChars "instead?"
                            ]
                        ]

                    suggestions ->
                        [ D.fillSep
                            [ D.fromChars "Maybe"
                            , D.fromChars "you"
                            , D.fromChars "want"
                            , D.fromChars "one"
                            , D.fromChars "of"
                            , D.fromChars "these"
                            , D.fromChars "instead?"
                            ]
                        , D.indent 4 <| D.green <| D.vcat suggestions
                        ]
                )


getNearbyFlags : String -> Flags a -> List ( Int, String ) -> List D.Doc
getNearbyFlags unknown flags unsortedFlags =
    -- case flags of
    --     FDone _ ->
    --         List.map D.fromChars <|
    --             List.map Tuple.second <|
    --                 List.sortBy Tuple.first <|
    --                     case List.filter (\( d, _ ) -> d < 3) unsortedFlags of
    --                         [] ->
    --                             unsortedFlags
    --                         nearbyUnsortedFlags ->
    --                             nearbyUnsortedFlags
    --     FMore more flag ->
    --         getNearbyFlags unknown more (getNearbyFlagsHelp unknown flag :: unsortedFlags)
    Debug.todo "getNearbyFlags"


getNearbyFlagsHelp : String -> Flag a -> ( Int, String )
getNearbyFlagsHelp unknown flag =
    case flag of
        OnOff flagName _ ->
            ( Suggest.distance unknown flagName
            , "--" ++ flagName
            )

        Flag flagName (Parser singular _ _ _ _) _ ->
            ( Suggest.distance unknown flagName
            , "--" ++ flagName ++ "=" ++ toToken singular
            )

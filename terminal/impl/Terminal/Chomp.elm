module Terminal.Chomp exposing (chomp)

import Data.IO as IO exposing (IO)
import Terminal.Internal exposing (ArgError(..), Args(..), CompleteArgs(..), Error(..), Expectation(..), Flag(..), FlagError(..), Flags(..), Parser(..), RequiredArgs(..))
import Utils



-- CHOMP INTERFACE


chomp : Maybe Int -> List String -> Args args -> Flags flags -> ( IO (List String), Result Error ( args, flags ) )
chomp maybeIndex strings args flags =
    let
        (Chomper flagChomper) =
            chompFlags flags
    in
    case flagChomper (toSuggest maybeIndex) (toChunks strings) of
        ChomperOk suggest chunks flagValue ->
            Tuple.mapSecond (Result.map (\a -> ( a, flagValue ))) (chompArgs suggest chunks args)

        ChomperErr suggest flagError ->
            ( addSuggest (IO.pure []) suggest, Err (BadFlag flagError) )


toChunks : List String -> List Chunk
toChunks strings =
    List.map2 Chunk
        (List.repeat (List.length strings) ()
            |> List.indexedMap (\i _ -> i)
        )
        strings


toSuggest : Maybe Int -> Suggest
toSuggest maybeIndex =
    case maybeIndex of
        Nothing ->
            NoSuggestion

        Just index ->
            Suggest index



-- CHOMPER


type Chomper x a
    = Chomper (Suggest -> List Chunk -> ChomperResult x a)


type ChomperResult x a
    = ChomperOk Suggest (List Chunk) a
    | ChomperErr Suggest x


type Chunk
    = Chunk Int String


type Suggest
    = NoSuggestion
    | Suggest Int
    | Suggestions (IO (List String))


makeSuggestion : Suggest -> (Int -> Maybe (IO (List String))) -> Suggest
makeSuggestion suggest maybeUpdate =
    case suggest of
        NoSuggestion ->
            suggest

        Suggestions _ ->
            suggest

        Suggest index ->
            Utils.maybe suggest Suggestions (maybeUpdate index)



-- ARGS


chompArgs : Suggest -> List Chunk -> Args a -> ( IO (List String), Result Error a )
chompArgs suggest chunks (Args completeArgsList) =
    chompArgsHelp suggest chunks completeArgsList [] []


chompArgsHelp : Suggest -> List Chunk -> List (CompleteArgs a) -> List Suggest -> List ( CompleteArgs a, ArgError ) -> ( IO (List String), Result Error a )
chompArgsHelp suggest chunks completeArgsList revSuggest revArgErrors =
    case completeArgsList of
        [] ->
            -- ( List.foldl (Utils.flip addSuggest) (IO.pure []) revSuggest
            -- , Err (BadArgs (List.reverse revArgErrors))
            -- )
            Debug.todo "chompArgsHelp"

        completeArgs :: others ->
            case chompCompleteArgs suggest chunks completeArgs of
                ( s1, Err argError ) ->
                    chompArgsHelp suggest chunks others (s1 :: revSuggest) (( completeArgs, argError ) :: revArgErrors)

                ( s1, Ok value ) ->
                    ( addSuggest (IO.pure []) s1
                    , Ok value
                    )


addSuggest : IO (List String) -> Suggest -> IO (List String)
addSuggest everything suggest =
    case suggest of
        NoSuggestion ->
            everything

        Suggest _ ->
            everything

        Suggestions newStuff ->
            IO.pure (++)
                |> IO.apply newStuff
                |> IO.apply everything



-- COMPLETE ARGS


chompCompleteArgs : Suggest -> List Chunk -> CompleteArgs a -> ( Suggest, Result ArgError a )
chompCompleteArgs suggest chunks completeArgs =
    let
        numChunks =
            List.length chunks
    in
    case completeArgs of
        Exactly requiredArgs ->
            chompExactly suggest chunks (chompRequiredArgs numChunks requiredArgs)

        Optional requiredArgs parser ->
            chompOptional suggest chunks (chompRequiredArgs numChunks requiredArgs) parser

        Multiple requiredArgs parser ->
            chompMultiple suggest chunks (chompRequiredArgs numChunks requiredArgs) parser


chompExactly : Suggest -> List Chunk -> Chomper ArgError a -> ( Suggest, Result ArgError a )
chompExactly suggest chunks (Chomper chomper) =
    case chomper suggest chunks of
        ChomperOk s cs value ->
            case List.map (\(Chunk _ chunk) -> chunk) cs of
                [] ->
                    ( s, Ok value )

                es ->
                    ( s, Err (ArgExtras es) )

        ChomperErr s argError ->
            ( s, Err argError )


chompOptional : Suggest -> List Chunk -> Chomper ArgError (Maybe a -> b) -> Parser a -> ( Suggest, Result ArgError b )
chompOptional suggest chunks (Chomper chomper) parser =
    -- let
    --     ok s1 cs func =
    --         case cs of
    --             [] ->
    --                 ( s1, Ok (func Nothing) )
    --             (Chunk index string) :: others ->
    --                 case tryToParse s1 parser index string of
    --                     ( s2, Err expectation ) ->
    --                         ( s2, Err (ArgBad string expectation) )
    --                     ( s2, Ok value ) ->
    --                         case List.map (\(Chunk _ chunk) -> chunk) others of
    --                             [] ->
    --                                 ( s2, Ok (func (Just value)) )
    --                             es ->
    --                                 ( s2, Err (ArgExtras es) )
    --     err s1 argError =
    --         ( s1, Err argError )
    -- in
    -- chomper suggest chunks ok err
    Debug.todo "chompOptional"


chompMultiple : Suggest -> List Chunk -> Chomper ArgError (List a -> b) -> Parser a -> ( Suggest, Result ArgError b )
chompMultiple suggest chunks (Chomper chomper) parser =
    -- let
    --     err s1 argError =
    --         ( s1, Err argError )
    -- in
    -- chomper suggest chunks (chompMultipleHelp parser []) err
    Debug.todo "chompMultiple"


chompMultipleHelp : Parser a -> List a -> Suggest -> List Chunk -> (List a -> b) -> ( Suggest, Result ArgError b )
chompMultipleHelp parser revArgs suggest chunks func =
    case chunks of
        [] ->
            ( suggest, Ok (func (List.reverse revArgs)) )

        (Chunk index string) :: otherChunks ->
            case tryToParse suggest parser index string of
                ( s1, Err expectation ) ->
                    ( s1, Err (ArgBad string expectation) )

                ( s1, Ok arg ) ->
                    chompMultipleHelp parser (arg :: revArgs) s1 otherChunks func



-- REQUIRED ARGS


chompRequiredArgs : Int -> RequiredArgs a -> Chomper ArgError a
chompRequiredArgs numChunks args =
    case args of
        Done value ->
            pure value

        Required funcArgs argParser ->
            -- chompRequiredArgs numChunks funcArgs
            --     |> bind
            --         (\func ->
            --             chompArg numChunks argParser
            --                 |> fmap (\arg -> func arg)
            --         )
            Debug.todo "chompRequiredArgs"


chompArg : Int -> Parser a -> Chomper ArgError a
chompArg numChunks ((Parser singular _ _ _ toExamples) as parser) =
    Chomper <|
        \suggest chunks ->
            case chunks of
                [] ->
                    let
                        newSuggest =
                            makeSuggestion suggest (suggestArg parser numChunks)

                        theError =
                            ArgMissing (Expectation singular (toExamples ""))
                    in
                    ChomperErr newSuggest theError

                (Chunk index string) :: otherChunks ->
                    case tryToParse suggest parser index string of
                        ( newSuggest, Err expectation ) ->
                            ChomperErr newSuggest (ArgBad string expectation)

                        ( newSuggest, Ok arg ) ->
                            ChomperOk newSuggest otherChunks arg


suggestArg : Parser a -> Int -> Int -> Maybe (IO (List String))
suggestArg (Parser _ _ _ toSuggestions _) numChunks targetIndex =
    if numChunks <= targetIndex then
        Just (toSuggestions "")

    else
        Nothing



-- PARSER


tryToParse : Suggest -> Parser a -> Int -> String -> ( Suggest, Result Expectation a )
tryToParse suggest (Parser singular _ parse toSuggestions toExamples) index string =
    let
        newSuggest =
            makeSuggestion suggest <|
                \targetIndex ->
                    if index == targetIndex then
                        Just (toSuggestions string)

                    else
                        Nothing

        outcome =
            case parse string of
                Nothing ->
                    Err (Expectation singular (toExamples string))

                Just value ->
                    Ok value
    in
    ( newSuggest, outcome )



-- FLAGS


chompFlags : Flags a -> Chomper FlagError a
chompFlags flags =
    chompFlagsHelp flags
        |> bind
            (\value ->
                checkForUnknownFlags flags
                    |> fmap (\_ -> value)
            )


chompFlagsHelp : Flags a -> Chomper FlagError a
chompFlagsHelp flags =
    case flags of
        FDone value ->
            pure value

        FMore funcFlags argFlag ->
            -- chompFlagsHelp funcFlags
            --     |> bind
            --         (\func ->
            --             chompFlag argFlag
            --                 |> fmap (\arg -> func arg)
            --         )
            Debug.todo "chompFlagsHelp"



-- FLAG


chompFlag : Flag a -> Chomper FlagError a
chompFlag flag =
    -- case flag of
    --     OnOff flagName _ ->
    --         chompOnOffFlag flagName
    --     Flag flagName parser _ ->
    --         chompNormalFlag flagName parser
    Debug.todo "chompFlag"


chompOnOffFlag : String -> Chomper FlagError Bool
chompOnOffFlag flagName =
    Chomper <|
        \suggest chunks ->
            case findFlag flagName chunks of
                Nothing ->
                    ChomperOk suggest chunks False

                Just (FoundFlag before value after) ->
                    case value of
                        DefNope ->
                            ChomperOk suggest (before ++ after) True

                        Possibly chunk ->
                            ChomperOk suggest (before ++ chunk :: after) True

                        Definitely _ string ->
                            ChomperErr suggest (FlagWithValue flagName string)


chompNormalFlag : String -> Parser a -> Chomper FlagError (Maybe a)
chompNormalFlag flagName ((Parser singular _ _ _ toExamples) as parser) =
    Chomper <|
        \suggest chunks ->
            case findFlag flagName chunks of
                Nothing ->
                    ChomperOk suggest chunks Nothing

                Just (FoundFlag before value after) ->
                    let
                        attempt index string =
                            case tryToParse suggest parser index string of
                                ( newSuggest, Err expectation ) ->
                                    ChomperErr newSuggest (FlagWithBadValue flagName string expectation)

                                ( newSuggest, Ok flagValue ) ->
                                    ChomperOk newSuggest (before ++ after) (Just flagValue)
                    in
                    case value of
                        Definitely index string ->
                            attempt index string

                        Possibly (Chunk index string) ->
                            attempt index string

                        DefNope ->
                            ChomperErr suggest (FlagWithNoValue flagName (Expectation singular (toExamples "")))



-- FIND FLAG


type FoundFlag
    = FoundFlag (List Chunk) Value (List Chunk)


type Value
    = Definitely Int String
    | Possibly Chunk
    | DefNope


findFlag : String -> List Chunk -> Maybe FoundFlag
findFlag flagName chunks =
    findFlagHelp [] ("--" ++ flagName) ("--" ++ flagName ++ "=") chunks


findFlagHelp : List Chunk -> String -> String -> List Chunk -> Maybe FoundFlag
findFlagHelp revPrev loneFlag flagPrefix chunks =
    let
        succeed value after =
            Just (FoundFlag (List.reverse revPrev) value after)

        deprefix string =
            String.dropLeft (String.length flagPrefix) string
    in
    case chunks of
        [] ->
            Nothing

        ((Chunk index string) as chunk) :: rest ->
            if String.startsWith flagPrefix string then
                succeed (Definitely index (deprefix string)) rest

            else if string /= loneFlag then
                findFlagHelp (chunk :: revPrev) loneFlag flagPrefix rest

            else
                case rest of
                    [] ->
                        succeed DefNope []

                    ((Chunk _ potentialArg) as argChunk) :: restOfRest ->
                        if String.startsWith "-" potentialArg then
                            succeed DefNope rest

                        else
                            succeed (Possibly argChunk) restOfRest



-- CHECK FOR UNKNOWN FLAGS


checkForUnknownFlags : Flags a -> Chomper FlagError ()
checkForUnknownFlags flags =
    Chomper <|
        \suggest chunks ->
            case List.filter startsWithDash chunks of
                [] ->
                    ChomperOk suggest chunks ()

                ((Chunk _ unknownFlag) :: _) as unknownFlags ->
                    -- ChomperErr
                    --     (makeSuggestion suggest (suggestFlag unknownFlags flags))
                    --     (FlagUnknown unknownFlag flags)
                    Debug.todo "checkForUnknownFlags"


suggestFlag : List Chunk -> Flags a -> Int -> Maybe (IO (List String))
suggestFlag unknownFlags flags targetIndex =
    case unknownFlags of
        [] ->
            Nothing

        (Chunk index string) :: otherUnknownFlags ->
            if index == targetIndex then
                Just (IO.pure (List.filter (String.startsWith string) (getFlagNames flags [])))

            else
                suggestFlag otherUnknownFlags flags targetIndex


startsWithDash : Chunk -> Bool
startsWithDash (Chunk _ string) =
    String.startsWith "-" string


getFlagNames : Flags a -> List String -> List String
getFlagNames flags names =
    case flags of
        FDone _ ->
            "--help" :: names

        FMore subFlags flag ->
            -- getFlagNames subFlags (getFlagName flag :: names)
            Debug.todo "getFlagNames"


getFlagName : Flag a -> String
getFlagName flag =
    case flag of
        Flag name _ _ ->
            "--" ++ name

        OnOff name _ ->
            "--" ++ name



-- CHOMPER INSTANCES


fmap : (a -> b) -> Chomper x a -> Chomper x b
fmap func (Chomper chomper) =
    Chomper <|
        \i w ->
            case chomper i w of
                ChomperOk s1 cs1 value ->
                    ChomperOk s1 cs1 (func value)

                ChomperErr sErr e ->
                    ChomperErr sErr e


pure : a -> Chomper x a
pure value =
    Chomper <|
        \ss cs ->
            ChomperOk ss cs value


apply : Chomper x a -> Chomper x (a -> b) -> Chomper x b
apply (Chomper argChomper) (Chomper funcChomper) =
    Chomper <|
        \s cs ->
            -- let
            --     ok1 s1 cs1 func =
            --         let
            --             ok2 s2 cs2 value =
            --                 ok s2 cs2 (func value)
            --         in
            --         argChomper s1 cs1 ok2 err
            -- in
            -- funcChomper s cs ok1 err
            Debug.todo "chomperApply"


bind : (a -> Chomper x b) -> Chomper x a -> Chomper x b
bind callback (Chomper aChomper) =
    Chomper <|
        \s cs ->
            case aChomper s cs of
                ChomperOk s1 cs1 a ->
                    case callback a of
                        Chomper bChomper ->
                            bChomper s1 cs1

                ChomperErr sErr e ->
                    ChomperErr sErr e

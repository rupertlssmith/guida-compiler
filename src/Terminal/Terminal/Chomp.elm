module Terminal.Terminal.Chomp exposing
    ( apply
    , bind
    , checkForUnknownFlags
    , chomp
    , chompArg
    , chompExactly
    , chompMultiple
    , chompNormalFlag
    , chompOnOffFlag
    , fmap
    , pure
    )

import Basics.Extra exposing (flip)
import Data.IO as IO exposing (IO)
import Data.Maybe as Maybe
import Terminal.Terminal.Internal exposing (ArgError(..), Args(..), CompleteArgs(..), Error(..), Expectation(..), Flag(..), FlagError(..), Flags(..), Parser(..), RequiredArgs(..))



-- CHOMP INTERFACE


chomp :
    Maybe Int
    -> List String
    -> List (Suggest -> List Chunk -> ( Suggest, Result ArgError args ))
    -> Chomper FlagError flags
    -> ( IO (List String), Result Error ( args, flags ) )
chomp maybeIndex strings args (Chomper flagChomper) =
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
            Maybe.maybe suggest Suggestions (maybeUpdate index)



-- ARGS


chompArgs : Suggest -> List Chunk -> List (Suggest -> List Chunk -> ( Suggest, Result ArgError a )) -> ( IO (List String), Result Error a )
chompArgs suggest chunks completeArgsList =
    chompArgsHelp suggest chunks completeArgsList [] []


chompArgsHelp :
    Suggest
    -> List Chunk
    -> List (Suggest -> List Chunk -> ( Suggest, Result ArgError a ))
    -> List Suggest
    -> List ArgError
    -> ( IO (List String), Result Error a )
chompArgsHelp suggest chunks completeArgsList revSuggest revArgErrors =
    case completeArgsList of
        [] ->
            ( List.foldl (flip addSuggest) (IO.pure []) revSuggest
            , Err (BadArgs (List.reverse revArgErrors))
            )

        completeArgs :: others ->
            case completeArgs suggest chunks of
                ( s1, Err argError ) ->
                    chompArgsHelp suggest chunks others (s1 :: revSuggest) (argError :: revArgErrors)

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


chompCompleteArgs : Suggest -> List Chunk -> CompleteArgs -> (String -> Maybe a) -> ( Suggest, Result ArgError a )
chompCompleteArgs suggest chunks completeArgs parserFn =
    let
        numChunks =
            List.length chunks
    in
    case completeArgs of
        Exactly requiredArgs ->
            chompExactly (chompRequiredArgs numChunks requiredArgs) suggest chunks

        Optional requiredArgs parser ->
            chompOptional (chompRequiredArgs numChunks requiredArgs) parser parserFn suggest chunks

        Multiple requiredArgs parser ->
            chompMultiple (chompRequiredArgs numChunks requiredArgs) parser parserFn suggest chunks


chompExactly : Chomper ArgError a -> Suggest -> List Chunk -> ( Suggest, Result ArgError a )
chompExactly (Chomper chomper) suggest chunks =
    case chomper suggest chunks of
        ChomperOk s cs value ->
            case List.map (\(Chunk _ chunk) -> chunk) cs of
                [] ->
                    ( s, Ok value )

                es ->
                    ( s, Err (ArgExtras es) )

        ChomperErr s argError ->
            ( s, Err argError )


chompOptional : Chomper ArgError (Maybe a -> b) -> Parser -> (String -> Maybe a) -> Suggest -> List Chunk -> ( Suggest, Result ArgError b )
chompOptional (Chomper chomper) parser parserFn suggest chunks =
    case chomper suggest chunks of
        ChomperOk s1 cs func ->
            case cs of
                [] ->
                    ( s1, Ok (func Nothing) )

                (Chunk index string) :: others ->
                    case tryToParse s1 parser parserFn index string of
                        ( s2, Err expectation ) ->
                            ( s2, Err (ArgBad string expectation) )

                        ( s2, Ok value ) ->
                            case List.map (\(Chunk _ chunk) -> chunk) others of
                                [] ->
                                    ( s2, Ok (func (Just value)) )

                                es ->
                                    ( s2, Err (ArgExtras es) )

        ChomperErr s1 argError ->
            ( s1, Err argError )


chompMultiple : Chomper ArgError (List a -> b) -> Parser -> (String -> Maybe a) -> Suggest -> List Chunk -> ( Suggest, Result ArgError b )
chompMultiple (Chomper chomper) parser parserFn suggest chunks =
    case chomper suggest chunks of
        ChomperOk s1 cs func ->
            chompMultipleHelp parser parserFn [] s1 cs func

        ChomperErr s1 argError ->
            ( s1, Err argError )


chompMultipleHelp : Parser -> (String -> Maybe a) -> List a -> Suggest -> List Chunk -> (List a -> b) -> ( Suggest, Result ArgError b )
chompMultipleHelp parser parserFn revArgs suggest chunks func =
    case chunks of
        [] ->
            ( suggest, Ok (func (List.reverse revArgs)) )

        (Chunk index string) :: otherChunks ->
            case tryToParse suggest parser parserFn index string of
                ( s1, Err expectation ) ->
                    ( s1, Err (ArgBad string expectation) )

                ( s1, Ok arg ) ->
                    chompMultipleHelp parser parserFn (arg :: revArgs) s1 otherChunks func



-- REQUIRED ARGS


chompRequiredArgs : Int -> RequiredArgs -> Chomper ArgError a
chompRequiredArgs numChunks args =
    -- case args of
    --     Done value ->
    --         pure value
    --     Required funcArgs argParser ->
    --         chompRequiredArgs numChunks funcArgs
    --             |> bind
    --                 (\func ->
    --                     chompArg numChunks argParser
    --                         |> fmap (\arg -> func arg)
    --                 )
    Debug.todo ("chompRequiredArgs: " ++ Debug.toString ( numChunks, args ))


chompArg : Int -> Parser -> (String -> Maybe a) -> Chomper ArgError a
chompArg numChunks ((Parser { singular, examples }) as parser) parserFn =
    Chomper <|
        \suggest chunks ->
            case chunks of
                [] ->
                    let
                        newSuggest =
                            makeSuggestion suggest (suggestArg parser numChunks)

                        theError =
                            ArgMissing (Expectation singular (examples ""))
                    in
                    ChomperErr newSuggest theError

                (Chunk index string) :: otherChunks ->
                    case tryToParse suggest parser parserFn index string of
                        ( newSuggest, Err expectation ) ->
                            ChomperErr newSuggest (ArgBad string expectation)

                        ( newSuggest, Ok arg ) ->
                            ChomperOk newSuggest otherChunks arg


suggestArg : Parser -> Int -> Int -> Maybe (IO (List String))
suggestArg (Parser { suggest }) numChunks targetIndex =
    if numChunks <= targetIndex then
        Just (suggest "")

    else
        Nothing



-- PARSER


tryToParse : Suggest -> Parser -> (String -> Maybe a) -> Int -> String -> ( Suggest, Result Expectation a )
tryToParse suggest (Parser parser) parserFn index string =
    let
        newSuggest =
            makeSuggestion suggest <|
                \targetIndex ->
                    if index == targetIndex then
                        Just (parser.suggest string)

                    else
                        Nothing

        outcome =
            case parserFn string of
                Nothing ->
                    Err (Expectation parser.singular (parser.examples string))

                Just value ->
                    Ok value
    in
    ( newSuggest, outcome )



-- FLAGS


chompFlags : Flags -> Chomper FlagError a
chompFlags flags =
    chompFlagsHelp flags
        |> bind
            (\value ->
                checkForUnknownFlags flags
                    |> fmap (\_ -> value)
            )


chompFlagsHelp : Flags -> Chomper FlagError a
chompFlagsHelp flags =
    -- case flags of
    --     FDone value ->
    --         pure value
    --     FMore funcFlags argFlag ->
    --         chompFlagsHelp funcFlags
    --             |> bind
    --                 (\func ->
    --                     chompFlag argFlag
    --                         |> fmap (\arg -> func arg)
    --                 )
    Debug.todo "chompFlagsHelp"



-- FLAG


chompFlag : Flag -> Chomper FlagError a
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


chompNormalFlag : String -> Parser -> (String -> Maybe a) -> Chomper FlagError (Maybe a)
chompNormalFlag flagName ((Parser { singular, examples }) as parser) parserFn =
    Chomper <|
        \suggest chunks ->
            case findFlag flagName chunks of
                Nothing ->
                    ChomperOk suggest chunks Nothing

                Just (FoundFlag before value after) ->
                    let
                        attempt index string =
                            case tryToParse suggest parser parserFn index string of
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
                            ChomperErr suggest (FlagWithNoValue flagName (Expectation singular (examples "")))



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


checkForUnknownFlags : Flags -> Chomper FlagError ()
checkForUnknownFlags flags =
    Chomper <|
        \suggest chunks ->
            case List.filter startsWithDash chunks of
                [] ->
                    ChomperOk suggest chunks ()

                ((Chunk _ unknownFlag) :: _) as unknownFlags ->
                    ChomperErr
                        (makeSuggestion suggest (suggestFlag unknownFlags flags))
                        (FlagUnknown unknownFlag flags)


suggestFlag : List Chunk -> Flags -> Int -> Maybe (IO (List String))
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


getFlagNames : Flags -> List String -> List String
getFlagNames flags names =
    case flags of
        FDone ->
            "--help" :: names

        FMore subFlags flag ->
            getFlagNames subFlags (getFlagName flag :: names)


getFlagName : Flag -> String
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
            let
                ok1 s1 cs1 func =
                    case argChomper s1 cs1 of
                        ChomperOk s2 cs2 value ->
                            ChomperOk s2 cs2 (func value)

                        ChomperErr s2 err ->
                            ChomperErr s2 err
            in
            case funcChomper s cs of
                ChomperOk s1 cs1 func ->
                    ok1 s1 cs1 func

                ChomperErr s1 err ->
                    ChomperErr s1 err


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

module Terminal.Internal exposing
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

import Data.IO exposing (IO)
import Reporting.Doc as D



-- ERROR


type Error
    = BadArgs (List ( CompleteArgs CompleteArgsValue, ArgError ))
    | BadFlag FlagError


type ArgError
    = ArgMissing Expectation
    | ArgBad String Expectation
    | ArgExtras (List String)


type FlagError
    = FlagWithValue String String
    | FlagWithBadValue String String Expectation
    | FlagWithNoValue String Expectation
    | FlagUnknown String (Flags FlagErrorValue)


type FlagErrorValue
    = FlagErrorValueUnit


type Expectation
    = Expectation String (IO (List String))



-- COMMAND


type Command
    = Command String Summary String D.Doc ArgDocs FlagDocs (List String -> Result Error (IO ()))


toName : Command -> String
toName (Command name _ _ _ _ _ _) =
    name


{-| The information that shows when you run the executable with no arguments.
If you say it is `Common`, you need to tell people what it does. Try to keep
it to two or three lines. If you say it is `Uncommon` you can rely on `Details`
for a more complete explanation.
-}
type Summary
    = Common String
    | Uncommon



-- FLAGS


type Flags a
    = FDone a
    | FMore (Flags (FlagValue -> a)) (Flag FlagValue)


type Flag a
    = Flag String (Parser a) String
    | OnOff String String


type FlagValue
    = FlagValueUnit


type FlagDocs
    = FlagDocs (List D.Doc)



-- PARSERS


type Parser a
    = Parser String String (String -> Maybe a) (String -> IO (List String)) (String -> IO (List String))



-- ARGS


type Args a
    = Args (List (CompleteArgs a))


type CompleteArgs args
    = Exactly (RequiredArgs args)
    | Multiple (RequiredArgs (List CompleteArgsValue -> args)) (Parser CompleteArgsValue)
    | Optional (RequiredArgs (Maybe CompleteArgsValue -> args)) (Parser CompleteArgsValue)


type CompleteArgsValue
    = CompleteArgsValueUnit


type RequiredArgs a
    = Done a
    | Required (RequiredArgs (RequiredArgsValue -> a)) (Parser RequiredArgsValue)


type RequiredArgsValue
    = RequiredArgsValueUnit


type ArgDocs
    = ArgDocs (String -> List D.Doc)

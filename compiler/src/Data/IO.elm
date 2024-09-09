module Data.IO exposing
    ( Effect(..)
    , ExitCode(..)
    , Handle(..)
    , IO(..)
    , IOError
    , IOException(..)
    , IOMode(..)
    , IORef(..)
    , Process(..)
    , StateT
    , apply
    , applyStateT
    , bind
    , bindStateT
    , catch
    , evalStateT
    , exitFailure
    , exitWith
    , fmap
    , fmapStateT
    , foldrM
    , getLine
    , gets
    , hClose
    , hFlush
    , hIsTerminalDevice
    , hPutStr
    , hPutStrLn
    , hSetEncoding
    , ioRefDecoder
    , ioRefEncoder
    , liftIO
    , mVectorGrow
    , mVectorLength
    , mVectorModify
    , mVectorRead
    , mVectorReplicate
    , mVectorWrite
    , make
    , modify
    , modifyIORef
    , newIORef
    , pure
    , pureStateT
    , putStr
    , readIORef
    , runStateT
    , stderr
    , stdout
    , utf8
    , vectorForM_
    , vectorImapM_
    , vectorUnsafeFreeze
    , vectorUnsafeInit
    , vectorUnsafeLast
    , withFile
    , writeIORef
    )

import Array exposing (Array)
import Array.Extra as Array
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe


make : Decode.Decoder a -> Effect -> IO a
make resultDecoder effect =
    IO
        (\next ->
            ( Process (Decode.map next resultDecoder)
            , effect
            , Nothing
            )
        )



-- EFFECT


type Effect
    = Exit String Int
    | NewIORef Encode.Value
    | ReadIORef Int
    | WriteIORef Int Encode.Value
    | GetLine
    | HPutStr Handle String
    | Write String Encode.Value
    | WriteString String String
    | PutStrLn String
    | DirDoesFileExist String
    | DirFindExecutable String
    | DirCreateDirectoryIfMissing Bool String
    | DirRemoveFile String
    | DirDoesDirectoryExist String
    | EnvLookupEnv String
    | EnvGetProgName
    | EnvGetArgs
    | BinaryDecodeFileOrFail String
    | Read String
    | HttpFetch String String
    | DirGetAppUserDataDirectory String
    | DirGetCurrentDirectory
    | DirGetModificationTime String
    | DirCanonicalizePath String
    | GetArchive String String
    | LockFile String
    | UnlockFile String
    | NewEmptyMVar
    | ReadMVar Int
    | TakeMVar Int
    | PutMVar Int Encode.Value
    | NoOp



-- IO


type IO a
    = IO ((a -> ( Process, Effect, Maybe (IO ()) )) -> ( Process, Effect, Maybe (IO ()) ))


type Process
    = Process (Decode.Decoder ( Process, Effect, Maybe (IO ()) ))


type IORef a
    = IORef Int


type TextEncoding
    = UTF8


utf8 : TextEncoding
utf8 =
    UTF8


catch : (e -> IO a) -> IO (Result e a) -> IO a
catch handler (IO io) =
    -- IO
    --     (\ioState ->
    --         case io ioState of
    --             ( newIoState, Ok a ) ->
    --                 ( newIoState, a )
    --             ( newIoState, Err e ) ->
    --                 case handler e of
    --                     IO newIo ->
    --                         newIo newIoState
    --     )
    Debug.todo "catch"


ioRefEncoder : IORef a -> Encode.Value
ioRefEncoder (IORef value) =
    Encode.int value


ioRefDecoder : Decode.Decoder (IORef a)
ioRefDecoder =
    Decode.map IORef Decode.int


newIORef : (a -> Encode.Value) -> a -> IO (IORef a)
newIORef encoder value =
    make (Decode.map IORef Decode.int) (NewIORef (encoder value))


readIORef : Decode.Decoder a -> IORef a -> IO a
readIORef decoder (IORef ref) =
    make decoder (ReadIORef ref)


writeIORef : (b -> Encode.Value) -> IORef a -> b -> IO ()
writeIORef encoder (IORef ref) value =
    make (Decode.succeed ()) (WriteIORef ref (encoder value))


modifyIORef : Decode.Decoder a -> (a -> Encode.Value) -> IORef a -> (a -> a) -> IO ()
modifyIORef decoder encoder ioRef func =
    readIORef decoder ioRef
        |> bind (\value -> writeIORef encoder ioRef (func value))


pure : a -> IO a
pure a =
    make (Decode.succeed a) NoOp


apply : IO a -> IO (a -> b) -> IO b
apply (IO ioArg) (IO ioFunc) =
    IO
        (\next ->
            ioArg
                (\a ->
                    ioFunc
                        (\aToB ->
                            next (aToB a)
                        )
                )
        )


fmap : (a -> b) -> IO a -> IO b
fmap fn (IO a) =
    IO (\k -> a (k << fn))


bind : (a -> IO b) -> IO a -> IO b
bind cont (IO fn) =
    IO
        (\next ->
            fn
                (\a ->
                    let
                        (IO cont2) =
                            cont a
                    in
                    cont2 next
                )
        )


foldrM : (a -> b -> IO b) -> b -> List a -> IO b
foldrM f z0 xs =
    let
        c x k z =
            bind k (f x z)
    in
    List.foldl c pure xs z0


mVectorReplicate : (a -> Encode.Value) -> Int -> a -> IO (IORef (Array (Maybe a)))
mVectorReplicate encoder n e =
    newIORef
        (Encode.array
            (Maybe.map encoder
                >> Maybe.withDefault Encode.null
            )
        )
        (Array.repeat n (Just e))


mVectorLength : IORef (Array (Maybe a)) -> IO Int
mVectorLength =
    readIORef (Decode.array (Decode.succeed Nothing))
        >> fmap Array.length


mVectorGrow : Decode.Decoder a -> (a -> Encode.Value) -> IORef (Array (Maybe a)) -> Int -> IO (IORef (Array (Maybe a)))
mVectorGrow decoder encoder ioRef length =
    readIORef (Decode.array (Decode.maybe decoder)) ioRef
        |> bind
            (\value ->
                writeIORef
                    (Encode.array
                        (Maybe.map encoder
                            >> Maybe.withDefault Encode.null
                        )
                    )
                    ioRef
                    (Array.append value (Array.repeat length Nothing))
            )
        |> fmap (\_ -> ioRef)


mVectorWrite : Decode.Decoder a -> (a -> Encode.Value) -> IORef (Array (Maybe a)) -> Int -> a -> IO ()
mVectorWrite decoder encoder ioRef i x =
    modifyIORef (Decode.array (Decode.maybe decoder))
        (Encode.array
            (Maybe.map encoder
                >> Maybe.withDefault Encode.null
            )
        )
        ioRef
        (Array.set i (Just x))


mVectorRead : Decode.Decoder a -> IORef (Array (Maybe a)) -> Int -> IO a
mVectorRead decoder ioRef i =
    readIORef (Decode.array (Decode.maybe decoder)) ioRef
        |> fmap
            (\vector ->
                case Maybe.join (Array.get i vector) of
                    Just a ->
                        a

                    Nothing ->
                        Debug.todo "Failed to find index on vector"
            )


vectorImapM_ : Decode.Decoder a -> (Int -> a -> IO b) -> IORef (Array (Maybe a)) -> IO ()
vectorImapM_ decoder action ioRef =
    readIORef (Decode.array (Decode.maybe decoder)) ioRef
        |> bind
            (\value ->
                Array.foldl
                    (\( i, maybeX ) ioAcc ->
                        case maybeX of
                            Just x ->
                                bind
                                    (\acc ->
                                        fmap (\newX -> Array.push (Just newX) acc)
                                            (action i x)
                                    )
                                    ioAcc

                            Nothing ->
                                ioAcc
                    )
                    (pure Array.empty)
                    (Array.indexedMap Tuple.pair value)
                    |> fmap (\_ -> ())
            )


vectorMapM_ : Decode.Decoder a -> (a -> IO b) -> IORef (Array (Maybe a)) -> IO ()
vectorMapM_ decoder action ioRef =
    vectorImapM_ decoder (\_ -> action) ioRef


vectorForM_ : Decode.Decoder a -> IORef (Array (Maybe a)) -> (a -> IO b) -> IO ()
vectorForM_ decoder ioRef action =
    vectorMapM_ decoder action ioRef


vectorUnsafeInit : IORef (Array (Maybe a)) -> IORef (Array (Maybe a))
vectorUnsafeInit =
    identity


mVectorModify : Decode.Decoder a -> (a -> Encode.Value) -> IORef (Array (Maybe a)) -> (a -> a) -> Int -> IO ()
mVectorModify decoder encoder ioRef func index =
    modifyIORef (Decode.array (Decode.maybe decoder))
        (Encode.array
            (Maybe.map encoder
                >> Maybe.withDefault Encode.null
            )
        )
        ioRef
        (Array.update index (Maybe.map func))


vectorUnsafeLast : Decode.Decoder a -> IORef (Array (Maybe a)) -> IO a
vectorUnsafeLast decoder ioRef =
    readIORef (Decode.array (Decode.maybe decoder)) ioRef
        |> fmap
            (\value ->
                case Maybe.join (Array.get (Array.length value - 1) value) of
                    Nothing ->
                        Debug.todo ("Failed to return last element of array (lenght: " ++ String.fromInt (Array.length value) ++ ")")

                    Just a ->
                        a
            )


vectorUnsafeFreeze : IORef (Array (Maybe a)) -> IO (IORef (Array (Maybe a)))
vectorUnsafeFreeze =
    pure



-- StateT


type StateT s a
    = StateT (s -> IO ( a, s ))


runStateT : StateT s a -> s -> IO ( a, s )
runStateT (StateT f) =
    f


evalStateT : StateT s a -> s -> IO a
evalStateT (StateT f) =
    f >> fmap Tuple.first


liftIO : IO a -> StateT s a
liftIO io =
    StateT (\s -> fmap (\a -> ( a, s )) io)


applyStateT : StateT s a -> StateT s (a -> b) -> StateT s b
applyStateT (StateT arg) (StateT func) =
    StateT
        (\s ->
            arg s
                |> bind
                    (\( a, sa ) ->
                        func sa
                            |> fmap (\( fb, sb ) -> ( fb a, sb ))
                    )
        )


fmapStateT : (a -> b) -> StateT s a -> StateT s b
fmapStateT func argStateT =
    applyStateT argStateT (pureStateT func)


bindStateT : (a -> StateT s b) -> StateT s a -> StateT s b
bindStateT func (StateT arg) =
    StateT
        (\s ->
            arg s
                |> bind
                    (\( a, sa ) ->
                        case func a of
                            StateT fb ->
                                fb sa
                    )
        )


pureStateT : a -> StateT s a
pureStateT value =
    StateT (\s -> pure ( value, s ))


gets : (s -> a) -> StateT s a
gets f =
    StateT (\s -> pure ( f s, s ))


modify : (s -> s) -> StateT s ()
modify f =
    StateT (\s -> pure ( (), f s ))



-- Handles


type Handle
    = Stdout
    | Stderr


stdout : Handle
stdout =
    Stdout


stderr : Handle
stderr =
    Stderr


hFlush : Handle -> IO ()
hFlush handle =
    make (Decode.succeed ()) NoOp


hClose : Handle -> IO ()
hClose handle =
    Debug.todo "hClose"


getLine : IO String
getLine =
    make Decode.string GetLine


putStr : String -> IO ()
putStr =
    hPutStr stdout


hPutStr : Handle -> String -> IO ()
hPutStr handle str =
    make (Decode.succeed ()) (HPutStr handle str)


hPutStrLn : Handle -> String -> IO ()
hPutStrLn handle str =
    hPutStr handle (str ++ "\n")


hIsTerminalDevice : Handle -> IO Bool
hIsTerminalDevice _ =
    pure True



-- IOMode


type IOMode
    = ReadMode
    | WriteMode
    | AppendMode
    | ReadWriteMode



-- IOError


type alias IOError =
    IOException



-- IOException


type IOException
    = IOException



-- ExitCode


type ExitCode
    = ExitSuccess
    | ExitFailure Int


exitWith : ExitCode -> IO a
exitWith _ =
    IO (\_ -> Debug.todo "exitWith")


exitFailure : IO a
exitFailure =
    exitWith (ExitFailure 1)


hSetEncoding : Handle -> TextEncoding -> IO ()
hSetEncoding _ _ =
    -- TODO review this
    pure ()


withFile : String -> IOMode -> (Handle -> IO a) -> IO a
withFile _ _ callback =
    -- TODO review this
    callback Stdout

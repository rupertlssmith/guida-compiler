module Builder.File exposing
    ( Time(..)
    , exists
    , getTime
    , readBinary
    , readUtf8
    , remove
    , removeDir
    , timeDecoder
    , timeEncoder
    , writeBinary
    , writeBuilder
    , writePackage
    , writeUtf8
    , zeroTime
    )

import Data.IO as IO exposing (IO(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Time
import Utils.Crash exposing (todo)
import Utils.Main as Utils exposing (FilePath, ZipArchive, ZipEntry)



-- TIME


type Time
    = Time Time.Posix


getTime : FilePath -> IO Time
getTime path =
    IO.fmap Time (Utils.dirGetModificationTime path)


zeroTime : Time
zeroTime =
    Time (Time.millisToPosix 0)



-- BINARY


writeBinary : (a -> Encode.Value) -> FilePath -> a -> IO ()
writeBinary encoder path value =
    let
        dir =
            Utils.fpDropFileName path
    in
    Utils.dirCreateDirectoryIfMissing True dir
        |> IO.bind (\_ -> Utils.binaryEncodeFile encoder path value)


readBinary : Decode.Decoder a -> FilePath -> IO (Maybe a)
readBinary decoder path =
    Utils.dirDoesFileExist path
        |> IO.bind
            (\pathExists ->
                if pathExists then
                    Utils.binaryDecodeFileOrFail decoder path
                        |> IO.bind
                            (\result ->
                                case result of
                                    Ok a ->
                                        IO.pure (Just a)

                                    Err ( offset, message ) ->
                                        IO.hPutStrLn IO.stderr
                                            (Utils.unlines
                                                [ "+-------------------------------------------------------------------------------"
                                                , "|  Corrupt File: " ++ path
                                                , "|   Byte Offset: " ++ String.fromInt offset
                                                , "|       Message: " ++ message
                                                , "|"
                                                , "| Please report this to https://github.com/elm/compiler/issues"
                                                , "| Trying to continue anyway."
                                                , "+-------------------------------------------------------------------------------"
                                                ]
                                            )
                                            |> IO.fmap (\_ -> Nothing)
                            )

                else
                    IO.pure Nothing
            )



-- WRITE UTF-8


writeUtf8 : FilePath -> String -> IO ()
writeUtf8 path content =
    IO.make (Decode.succeed ()) (IO.WriteString path content)



-- READ UTF-8


readUtf8 : FilePath -> IO String
readUtf8 path =
    IO.make Decode.string (IO.Read path)


useZeroIfNotRegularFile : IO.IOException -> IO Int
useZeroIfNotRegularFile _ =
    IO.pure 0


hGetContentsSizeHint : IO.Handle -> Int -> Int -> IO String
hGetContentsSizeHint handle =
    -- let
    --     readChunks chunks readSize incrementSize =
    --         BS.mallocByteString readSize
    --             |> IO.bind
    --                 (\fp ->
    --                     FPtr.withForeignPtr fp <|
    --                         \buf ->
    --                             IO.hGetBuf handle buf readSize
    --                                 |> IO.bind
    --                                     (\readCount ->
    --                                         let
    --                                             chunk =
    --                                                 BS.PS fp 0 readCount
    --                                         in
    --                                         if readCount < readSize && readSize > 0 then
    --                                             return <| BS.concat (reverse (chunk :: chunks))
    --                                         else
    --                                             readChunks (chunk :: chunks) incrementSize (min 32752 (readSize + incrementSize))
    --                                     )
    --                 )
    -- in
    -- readChunks []
    todo "hGetContentsSizeHint"


encodingError : FilePath -> IO.IOError -> IO.IOError
encodingError path ioErr =
    -- case ioeGetErrorType ioErr of
    --     InvalidArgument ->
    --         annotateIOError
    --             (userError "Bad encoding; the file must be valid UTF-8")
    --             ""
    --             Nothing
    --             (Just path)
    --     _ ->
    --         ioErr
    todo "encodingError"



-- WRITE BUILDER


writeBuilder : FilePath -> String -> IO ()
writeBuilder path builder =
    IO.make (Decode.succeed ()) (IO.WriteString path builder)



-- WRITE PACKAGE


writePackage : FilePath -> ZipArchive -> IO ()
writePackage destination archive =
    case Utils.zipZEntries archive of
        [] ->
            IO.pure ()

        entry :: entries ->
            let
                root =
                    String.length (Utils.zipERelativePath entry)
            in
            Utils.mapM_ (writeEntry destination root) entries


writeEntry : FilePath -> Int -> ZipEntry -> IO ()
writeEntry destination root entry =
    let
        path =
            String.dropLeft root (Utils.zipERelativePath entry)
    in
    if
        String.startsWith "src/" path
            || (path == "LICENSE")
            || (path == "README.md")
            || (path == "elm.json")
    then
        if not (String.isEmpty path) && String.endsWith "/" path then
            Utils.dirCreateDirectoryIfMissing True (Utils.fpForwardSlash destination path)

        else
            writeUtf8 (Utils.fpForwardSlash destination path) (Utils.zipFromEntry entry)

    else
        IO.pure ()



-- EXISTS


exists : FilePath -> IO Bool
exists path =
    Utils.dirDoesFileExist path



-- REMOVE FILES


remove : FilePath -> IO ()
remove path =
    Utils.dirDoesFileExist path
        |> IO.bind
            (\exists_ ->
                if exists_ then
                    Utils.dirRemoveFile path

                else
                    IO.pure ()
            )


removeDir : FilePath -> IO ()
removeDir path =
    Utils.dirDoesFileExist path
        |> IO.bind
            (\exists_ ->
                if exists_ then
                    Utils.dirRemoveDirectoryRecursive path

                else
                    IO.pure ()
            )



-- ENCODERS and DECODERS


timeEncoder : Time -> Encode.Value
timeEncoder (Time posix) =
    Encode.int (Time.posixToMillis posix)


timeDecoder : Decode.Decoder Time
timeDecoder =
    Decode.map (Time << Time.millisToPosix) Decode.int

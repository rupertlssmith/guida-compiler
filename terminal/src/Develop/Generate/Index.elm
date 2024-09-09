module Develop.Generate.Index exposing (generate)

import AssocList as Dict exposing (Dict)
import BackgroundWriter as BW
import Data.IO as IO exposing (IO)
import Develop.Generate.Help as Help
import Elm.Details as Details
import Elm.Outline as Outline
import Elm.Package as Pkg
import Elm.Version as V
import Json.EncodeX as E
import Reporting
import Stuff
import Utils exposing (FilePath)



-- GENERATE


generate : FilePath -> IO String
generate pwd =
    getFlags pwd
        |> IO.fmap
            (\flags ->
                Help.makePageHtml "Index" (Just (encode flags))
            )



-- FLAGS


type Flags
    = Flags FilePath (List String) (List String) (List File) (Maybe String) (Maybe Outline.Outline) (Dict Pkg.Name V.Version)


type File
    = File FilePath Bool



-- GET FLAGS


getFlags : FilePath -> IO Flags
getFlags pwd =
    Utils.dirGetDirectoryContents pwd
        |> IO.bind
            (\contents ->
                Utils.dirGetCurrentDirectory
                    |> IO.bind
                        (\root ->
                            getDirs pwd contents
                                |> IO.bind
                                    (\dirs ->
                                        getFiles pwd contents
                                            |> IO.bind
                                                (\files ->
                                                    getReadme pwd
                                                        |> IO.bind
                                                            (\readme ->
                                                                getOutline
                                                                    |> IO.bind
                                                                        (\outline ->
                                                                            getExactDeps outline
                                                                                |> IO.fmap
                                                                                    (\exactDeps ->
                                                                                        Flags root
                                                                                            (List.dropWhile ((==) ".") (Utils.fpSplitDirectories pwd))
                                                                                            dirs
                                                                                            files
                                                                                            readme
                                                                                            outline
                                                                                            exactDeps
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )



-- README


getReadme : FilePath -> IO (Maybe String)
getReadme dir =
    let
        readmePath =
            dir ++ "/README.md"
    in
    Utils.dirDoesFileExist readmePath
        |> IO.bind
            (\exists ->
                if exists then
                    IO.fmap Just (Utils.readFile readmePath)

                else
                    IO.pure Nothing
            )



-- GET DIRECTORIES


getDirs : FilePath -> List FilePath -> IO (List FilePath)
getDirs pwd contents =
    Utils.filterM (Utils.dirDoesDirectoryExist << Utils.fpForwardSlash pwd) contents



-- GET FILES


getFiles : FilePath -> List FilePath -> IO (List File)
getFiles pwd contents =
    Utils.filterM (Utils.dirDoesFileExist << Utils.fpForwardSlash pwd) contents
        |> IO.bind
            (\paths ->
                Utils.mapM (toFile pwd) paths
            )


toFile : FilePath -> FilePath -> IO File
toFile pwd path =
    if Utils.fpTakeExtension path == ".elm" then
        Utils.readFile (Utils.fpForwardSlash pwd path)
            |> IO.fmap
                (\source ->
                    let
                        hasMain =
                            String.contains "\nmain " source
                    in
                    File path hasMain
                )

    else
        IO.pure (File path False)



-- GET OUTLINE


getOutline : IO (Maybe Outline.Outline)
getOutline =
    Stuff.findRoot
        |> IO.bind
            (\maybeRoot ->
                case maybeRoot of
                    Nothing ->
                        IO.pure Nothing

                    Just root ->
                        Outline.read root
                            |> IO.fmap
                                (\result ->
                                    case result of
                                        Err _ ->
                                            Nothing

                                        Ok outline ->
                                            Just outline
                                )
            )



-- GET EXACT DEPS


{-| TODO revamp how `elm reactor` works so that this can go away.
I am trying to "just get it working again" at this point though.
-}
getExactDeps : Maybe Outline.Outline -> IO (Dict Pkg.Name V.Version)
getExactDeps maybeOutline =
    case maybeOutline of
        Nothing ->
            IO.pure Dict.empty

        Just outline ->
            case outline of
                Outline.App _ ->
                    IO.pure Dict.empty

                Outline.Pkg _ ->
                    Stuff.findRoot
                        |> IO.bind
                            (\maybeRoot ->
                                case maybeRoot of
                                    Nothing ->
                                        IO.pure Dict.empty

                                    Just root ->
                                        BW.withScope
                                            (\scope ->
                                                Details.load Reporting.silent scope root
                                                    |> IO.fmap
                                                        (\result ->
                                                            case result of
                                                                Err _ ->
                                                                    Dict.empty

                                                                Ok (Details.Details _ validOutline _ _ _ _) ->
                                                                    case validOutline of
                                                                        Details.ValidApp _ ->
                                                                            Dict.empty

                                                                        Details.ValidPkg _ _ solution ->
                                                                            solution
                                                        )
                                            )
                            )



-- ENCODE


encode : Flags -> E.Value
encode (Flags root pwd dirs files readme outline exactDeps) =
    E.object
        [ ( "root", encodeFilePath root )
        , ( "pwd", E.list encodeFilePath pwd )
        , ( "dirs", E.list encodeFilePath dirs )
        , ( "files", E.list encodeFile files )
        , ( "readme", Utils.maybe E.null E.string readme )
        , ( "outline", Utils.maybe E.null Outline.encode outline )
        , ( "exactDeps", E.dict Pkg.toJsonString V.encode exactDeps )
        ]


encodeFilePath : FilePath -> E.Value
encodeFilePath filePath =
    E.string filePath


encodeFile : File -> E.Value
encodeFile (File path hasMain) =
    E.object
        [ ( "name", encodeFilePath path )
        , ( "runnable", E.bool hasMain )
        ]

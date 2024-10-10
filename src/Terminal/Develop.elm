module Terminal.Develop exposing
    ( Flags(..)
    , run
    )

import Builder.BackgroundWriter as BW
import Builder.Build as Build
import Builder.Elm.Details as Details
import Builder.Generate
import Builder.Reporting
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Builder.Stuff
import Compiler.Data.NonEmptyList as NE
import Compiler.Generate.Html as Html
import Data.IO as IO exposing (IO)
import Data.Map as Dict exposing (Dict)
import Data.Maybe as Maybe
import Prelude
import Terminal.Develop.Generate.Help as Help
import Terminal.Develop.Generate.Index as Index
import Terminal.Develop.StaticFiles as StaticFiles
import Utils.Crash exposing (todo)
import Utils.Main as Utils exposing (FilePath)



-- RUN THE DEV SERVER


type Flags
    = Flags (Maybe Int)


run : () -> Flags -> IO ()
run () (Flags maybePort) =
    let
        port_ =
            Maybe.maybe 8000 identity maybePort
    in
    Prelude.putStrLn ("Go to http://localhost:" ++ String.fromInt port_ ++ " to see your project dashboard.")
        |> IO.bind
            (\_ ->
                -- Utils.httpServe (config port_)
                --     (serveFiles
                --         (Utils.serveDirectoryWith directoryConfig ".")
                --         serveAssets
                --         error404
                --     )
                todo "run"
            )


config : Int -> Utils.HttpServerConfig
config port_ =
    let
        defaultConfig =
            Utils.defaultHttpServerConfig
    in
    -- setVerbose False <|
    --     setPort port_ <|
    --         setAccessLog ConfigNoLog <|
    --             setErrorLog ConfigNoLog <|
    --                 defaultConfig
    { defaultConfig
        | verbose = Just False
        , port_ = Just port_
    }



-- INDEX
-- directoryConfig : Utils.DirectoryConfig m
-- directoryConfig =
--     fancyDirectoryConfig
--         { indexFiles = []
--         , indexGenerator =
--             \pwd ->
--                 do modifyResponse <|
--                     setContentType "text/html;charset=utf-8"
--                         writeBuilder
--                         =<< liftIO (Index.generate pwd)
--         }
-- NOT FOUND
-- error404 : Snap ()
-- error404 =
--     do modifyResponse <|
--         setResponseStatus 404
--             "Not Found"
--             modifyResponse
--         <|
--             setContentType "text/html;charset=utf-8"
--                 writeBuilder
--             <|
--                 Help.makePageHtml "NotFound" Nothing
-- SERVE FILES


serveFiles : Utils.HttpServerSnap ()
serveFiles =
    -- getSafePath
    --     |> Snap.bind
    --         (\path ->
    --             Snap.bind guard (liftIO (Utils.dirDoesFileExist path))
    --                 |> Snap.bind (serveElm path (serveFilePretty path))
    --         )
    todo "serveFiles"



-- SERVE FILES + CODE HIGHLIGHTING
-- serveFilePretty : FilePath -> Snap ()
-- serveFilePretty path =
--     let
--         possibleExtensions =
--             getSubExts (takeExtensions path)
--     in
--     case mconcat (map lookupMimeType possibleExtensions) of
--         Nothing ->
--             serveCode path
--         Just mimeType ->
--             serveFileAs mimeType path


getSubExts : String -> List String
getSubExts fullExtension =
    if String.isEmpty fullExtension then
        []

    else
        fullExtension :: getSubExts (Utils.fpTakeExtension (String.dropLeft 1 fullExtension))



-- serveCode : String -> Snap ()
-- serveCode path =
--     liftIO (BS.readFile path)
--         |> IO.bind
--             (\code ->
--                 modifyResponse (setContentType "text/html")
--                     |> IO.bind
--                         (\_ ->
--                             writeBuilder <|
--                                 Help.makeCodeHtml ('~' :: '/' :: path) (B.byteString code)
--                         )
--             )
-- SERVE ELM
-- serveElm : FilePath -> Snap ()
-- serveElm path =
--     guard (takeExtension path == ".elm")
--         |> IO.bind (\_ -> modifyResponse (setContentType "text/html"))
--         |> IO.bind (\_ -> liftIO <| compile path)
--         |> IO.bind
--             (\result ->
--                 case result of
--                     Ok builder ->
--                         writeBuilder builder
--                     Err exit ->
--                         writeBuilder <|
--                             Help.makePageHtml "Errors" <|
--                                 Just <|
--                                     Exit.toJson <|
--                                         Exit.reactorToReport exit
--             )
-- compile : FilePath -> IO (Result Exit.Reactor String)
-- compile path =
--     Stuff.findRoot
--         |> IO.bind
--             (\maybeRoot ->
--                 case maybeRoot of
--                     Nothing ->
--                         IO.pure <| Err <| Exit.ReactorNoOutline
--                     Just root ->
--                         BW.withScope
--                             (\scope ->
--                                 Stuff.withRootLock root <|
--                                     Task.run <|
--                                         (Task.eio Exit.ReactorBadDetails (Details.load Reporting.silent scope root)
--                                             |> IO.bind
--                                                 (\details ->
--                                                     Task.eio Exit.ReactorBadBuild (Build.fromPaths Reporting.silent root details (NE.Nonempty path []))
--                                                         |> IO.bind
--                                                             (\artifacts ->
--                                                                 Task.mapError Exit.ReactorBadGenerate (Generate.dev root details artifacts)
--                                                                     |> IO.fmap
--                                                                         (\javascript ->
--                                                                             let
--                                                                                 (NE.Nonempty name _) =
--                                                                                     Build.getRootNames artifacts
--                                                                             in
--                                                                             Html.sandwich name javascript
--                                                                         )
--                                                             )
--                                                 )
--                                         )
--                             )
--             )
-- SERVE STATIC ASSETS
-- serveAssets : Snap ()
-- serveAssets =
--     getSafePath
--         |> Snap.bind
--             (\path ->
--                 case StaticFiles.lookup path of
--                     Nothing ->
--                         pass
--                     Just ( content, mimeType ) ->
--                         modifyResponse (setContentType (mimeType ++ ";charset=utf-8"))
--                             |> Snap.bind (\_ -> writeBS content)
--             )
-- MIME TYPES


lookupMimeType : FilePath -> Maybe String
lookupMimeType ext =
    Dict.get ext mimeTypeDict


mimeTypeDict : Dict FilePath String
mimeTypeDict =
    Dict.fromList compare
        [ ( ".asc", "text/plain" )
        , ( ".asf", "video/x-ms-asf" )
        , ( ".asx", "video/x-ms-asf" )
        , ( ".avi", "video/x-msvideo" )
        , ( ".bz2", "application/x-bzip" )
        , ( ".css", "text/css" )
        , ( ".dtd", "text/xml" )
        , ( ".dvi", "application/x-dvi" )
        , ( ".gif", "image/gif" )
        , ( ".gz", "application/x-gzip" )
        , ( ".htm", "text/html" )
        , ( ".html", "text/html" )
        , ( ".ico", "image/x-icon" )
        , ( ".jpeg", "image/jpeg" )
        , ( ".jpg", "image/jpeg" )
        , ( ".js", "text/javascript" )
        , ( ".json", "application/json" )
        , ( ".m3u", "audio/x-mpegurl" )
        , ( ".mov", "video/quicktime" )
        , ( ".mp3", "audio/mpeg" )
        , ( ".mp4", "video/mp4" )
        , ( ".mpeg", "video/mpeg" )
        , ( ".mpg", "video/mpeg" )
        , ( ".ogg", "application/ogg" )
        , ( ".otf", "font/otf" )
        , ( ".pac", "application/x-ns-proxy-autoconfig" )
        , ( ".pdf", "application/pdf" )
        , ( ".png", "image/png" )
        , ( ".qt", "video/quicktime" )
        , ( ".sfnt", "font/sfnt" )
        , ( ".sig", "application/pgp-signature" )
        , ( ".spl", "application/futuresplash" )
        , ( ".svg", "image/svg+xml" )
        , ( ".swf", "application/x-shockwave-flash" )
        , ( ".tar", "application/x-tar" )
        , ( ".tar.bz2", "application/x-bzip-compressed-tar" )
        , ( ".tar.gz", "application/x-tgz" )
        , ( ".tbz", "application/x-bzip-compressed-tar" )
        , ( ".text", "text/plain" )
        , ( ".tgz", "application/x-tgz" )
        , ( ".ttf", "font/ttf" )
        , ( ".txt", "text/plain" )
        , ( ".wav", "audio/x-wav" )
        , ( ".wax", "audio/x-ms-wax" )
        , ( ".webm", "video/webm" )
        , ( ".webp", "image/webp" )
        , ( ".wma", "audio/x-ms-wma" )
        , ( ".wmv", "video/x-ms-wmv" )
        , ( ".woff", "font/woff" )
        , ( ".woff2", "font/woff2" )
        , ( ".xbm", "image/x-xbitmap" )
        , ( ".xml", "text/xml" )
        , ( ".xpm", "image/x-xpixmap" )
        , ( ".xwd", "image/x-xwindowdump" )
        , ( ".zip", "application/zip" )
        ]

module Publish exposing (run)

import BackgroundWriter as BW
import Build
import Data.IO as IO exposing (IO)
import Data.NonEmptyList as NE
import Deps.Bump as Bump
import Deps.Diff as Diff
import Deps.Registry as Registry
import Deps.Website as Website
import Elm.Details as Details
import Elm.Docs as Docs
import Elm.Magnitude as M
import Elm.Outline as Outline
import Elm.Package as Pkg
import Elm.Version as V
import File
import Http
import Json.Decode as D
import Json.StringX as Json
import Reporting
import Reporting.Doc as D
import Reporting.Exit as Exit
import Reporting.Exit.Help as Help
import Reporting.Task as Task
import Stuff
import Utils.Main as Utils exposing (FilePath)



-- RUN
-- TODO mandate no "exposing (..)" in packages to make
-- optimization to skip builds in Elm.Details always valid


run : () -> () -> IO ()
run () () =
    Reporting.attempt Exit.publishToReport <|
        Task.run (Task.bind publish getEnv)



-- ENV


type Env
    = Env FilePath Stuff.PackageCache Http.Manager Registry.Registry Outline.Outline


getEnv : Task.Task Exit.Publish Env
getEnv =
    Task.mio Exit.PublishNoOutline Stuff.findRoot
        |> Task.bind
            (\root ->
                Task.io Stuff.getPackageCache
                    |> Task.bind
                        (\cache ->
                            Task.io Http.getManager
                                |> Task.bind
                                    (\manager ->
                                        Task.eio Exit.PublishMustHaveLatestRegistry (Registry.latest manager cache)
                                            |> Task.bind
                                                (\registry ->
                                                    Task.eio Exit.PublishBadOutline (Outline.read root)
                                                        |> Task.fmap
                                                            (\outline ->
                                                                Env root cache manager registry outline
                                                            )
                                                )
                                    )
                        )
            )



-- PUBLISH


publish : Env -> Task.Task Exit.Publish ()
publish ((Env root _ manager registry outline) as env) =
    case outline of
        Outline.App _ ->
            Task.throw Exit.PublishApplication

        Outline.Pkg (Outline.PkgOutline pkg summary _ vsn exposed _ _ _) ->
            let
                maybeKnownVersions =
                    Registry.getVersions pkg registry
            in
            reportPublishStart pkg vsn maybeKnownVersions
                |> IO.bind
                    (\_ ->
                        if noExposed exposed then
                            Task.throw Exit.PublishNoExposed

                        else
                            IO.pure ()
                    )
                |> IO.bind
                    (\_ ->
                        if badSummary summary then
                            Task.throw Exit.PublishNoSummary

                        else
                            IO.pure ()
                    )
                |> IO.bind (\_ -> verifyReadme root)
                |> IO.bind (\_ -> verifyLicense root)
                |> IO.bind (\_ -> verifyBuild root)
                |> IO.bind
                    (\docs ->
                        verifyVersion env pkg vsn docs maybeKnownVersions
                            |> IO.bind (\_ -> getGit)
                            |> IO.bind
                                (\git ->
                                    verifyTag git manager pkg vsn
                                        |> IO.bind
                                            (\commitHash ->
                                                verifyNoChanges git commitHash vsn
                                                    |> IO.bind
                                                        (\_ ->
                                                            verifyZip env pkg vsn
                                                                |> IO.bind
                                                                    (\zipHash ->
                                                                        Task.io (putStrLn "")
                                                                            |> IO.bind (\_ -> register manager pkg vsn docs commitHash zipHash)
                                                                            |> IO.bind (\_ -> Task.io (putStrLn "Success!"))
                                                                    )
                                                        )
                                            )
                                )
                    )



-- VERIFY SUMMARY


badSummary : String -> Bool
badSummary summary =
    Json.isEmpty summary || Outline.defaultSummary == summary


noExposed : Outline.Exposed -> Bool
noExposed exposed =
    case exposed of
        Outline.ExposedList modules ->
            List.isEmpty modules

        Outline.ExposedDict chunks ->
            List.all (List.isEmpty << Tuple.second) chunks



-- VERIFY README


verifyReadme : String -> Task.Task Exit.Publish ()
verifyReadme root =
    let
        readmePath =
            root ++ "/README.md"
    in
    reportReadmeCheck <|
        (File.exists readmePath
            |> IO.bind
                (\exists ->
                    if exists then
                        IO.withFile readmePath IO.ReadMode IO.hFileSize
                            |> IO.fmap
                                (\size ->
                                    if size < 300 then
                                        Err Exit.PublishShortReadme

                                    else
                                        Ok ()
                                )

                    else
                        IO.pure (Err Exit.PublishNoReadme)
                )
        )



-- VERIFY LICENSE


verifyLicense : String -> Task.Task Exit.Publish ()
verifyLicense root =
    let
        licensePath =
            root ++ "/LICENSE"
    in
    reportLicenseCheck <|
        (File.exists licensePath
            |> IO.fmap
                (\exists ->
                    if exists then
                        Ok ()

                    else
                        Err Exit.PublishNoLicense
                )
        )



-- VERIFY BUILD


verifyBuild : String -> Task.Task Exit.Publish Docs.Documentation
verifyBuild root =
    reportBuildCheck <|
        BW.withScope <|
            \scope ->
                Task.run <|
                    Task.eio Exit.PublishBadDetails
                        (Details.load Reporting.silent scope root)
                        |> IO.bind
                            (\((Details.Details _ outline _ _ _ _) as details) ->
                                (case outline of
                                    Details.ValidApp _ ->
                                        Task.throw Exit.PublishApplication

                                    Details.ValidPkg _ [] _ ->
                                        Task.throw Exit.PublishNoExposed

                                    Details.ValidPkg _ (e :: es) _ ->
                                        return (NE.Nonempty e es)
                                )
                                    |> IO.bind
                                        (\exposed ->
                                            Task.eio Exit.PublishBuildProblem <|
                                                Build.fromExposed Reporting.silent root details Build.KeepDocs exposed
                                        )
                            )



-- GET GIT


type Git
    = Git (List String -> IO Exit.ExitCode)


getGit : Task.Task Exit.Publish Git
getGit =
    Task.io (Dir.findExecutable "git")
        |> IO.bind
            (\maybeGit ->
                case maybeGit of
                    Nothing ->
                        Task.throw Exit.PublishNoGit

                    Just git ->
                        IO.pure <|
                            Git
                                (\args ->
                                    let
                                        process =
                                            Process.proc git
                                                args
                                                { std_in = Process.CreatePipe
                                                , std_out = Process.CreatePipe
                                                , std_err = Process.CreatePipe
                                                }
                                    in
                                    Process.withCreateProcess process
                                        (\_ _ _ handle ->
                                            Process.waitForProcess handle
                                        )
                                )
            )



-- VERIFY GITHUB TAG


verifyTag : Git -> Http.Manager -> Pkg.Name -> V.Version -> Task.Task Exit.Publish String
verifyTag git manager pkg vsn =
    reportTagCheck vsn <|
        (-- https://stackoverflow.com/questions/1064499/how-to-list-all-git-tags
         run_ git [ "show", "--name-only", V.toChars vsn, "--" ]
            |> IO.bind
                (\exitCode ->
                    case exitCode of
                        Exit.ExitFailure _ ->
                            IO.pure (Err (Exit.PublishMissingTag vsn))

                        Exit.ExitSuccess ->
                            let
                                url =
                                    toTagUrl pkg vsn
                            in
                            Http.get manager url [ Http.accept "application/json" ] (Exit.PublishCannotGetTag vsn) <|
                                \body ->
                                    case D.fromByteString commitHashDecoder body of
                                        Ok hash ->
                                            IO.pure <| Ok hash

                                        Err _ ->
                                            IO.pure <| Err (Exit.PublishCannotGetTagData vsn url body)
                )
        )


toTagUrl : Pkg.Name -> V.Version -> String
toTagUrl pkg vsn =
    "https://api.github.com/repos/" ++ Pkg.toUrl pkg ++ "/git/refs/tags/" ++ V.toChars vsn


commitHashDecoder : D.Decoder e String
commitHashDecoder =
    D.map Utf8.toChars (D.field "object" (D.field "sha" D.string))



-- VERIFY NO LOCAL CHANGES SINCE TAG


verifyNoChanges : Git -> String -> V.Version -> Task.Task Exit.Publish ()
verifyNoChanges git commitHash vsn =
    reportLocalChangesCheck <|
        (-- https://stackoverflow.com/questions/3878624/how-do-i-programmatically-determine-if-there-are-uncommited-changes
         run_ git [ "diff-index", "--quiet", commitHash, "--" ]
            |> IO.fmap
                (\exitCode ->
                    case exitCode of
                        Exit.ExitSuccess ->
                            Ok ()

                        Exit.ExitFailure _ ->
                            Err (Exit.PublishLocalChanges vsn)
                )
        )



-- VERIFY THAT ZIP BUILDS / COMPUTE HASH


verifyZip : Env -> Pkg.Name -> V.Version -> Task.Task Exit.Publish Http.Sha
verifyZip (Env root _ manager _ _) pkg vsn =
    withPrepublishDir root <|
        \prepublishDir ->
            let
                url =
                    toZipUrl pkg vsn
            in
            reportDownloadCheck
                (Http.getArchive manager
                    url
                    Exit.PublishCannotGetZip
                    (Exit.PublishCannotDecodeZip url)
                    (IO.pure << Ok)
                )
                |> IO.bind
                    (\( sha, archive ) ->
                        Task.io (File.writePackage prepublishDir archive)
                            |> IO.bind
                                (\_ ->
                                    reportZipBuildCheck <|
                                        Dir.withCurrentDirectory prepublishDir <|
                                            verifyZipBuild prepublishDir
                                )
                            |> IO.fmap (\_ -> sha)
                    )


toZipUrl : Pkg.Name -> V.Version -> String
toZipUrl pkg vsn =
    "https://github.com/" ++ Pkg.toUrl pkg ++ "/zipball/" ++ V.toChars vsn ++ "/"


withPrepublishDir : String -> (String -> Task.Task x a) -> Task.Task x a
withPrepublishDir root callback =
    let
        dir =
            Stuff.prepublishDir root
    in
    Task.eio id <|
        bracket_
            (Dir.createDirectoryIfMissing True dir)
            (Dir.removeDirectoryRecursive dir)
            (Task.run (callback dir))


verifyZipBuild : FilePath -> IO (Result Exit.Publish ())
verifyZipBuild root =
    BW.withScope
        (\scope ->
            Task.run
                (Task.eio Exit.PublishZipBadDetails
                    (Details.load Reporting.silent scope root)
                    |> IO.bind
                        (\((Details.Details _ outline _ _ _ _) as details) ->
                            (case outline of
                                Details.ValidApp _ ->
                                    Task.throw Exit.PublishZipApplication

                                Details.ValidPkg _ [] _ ->
                                    Task.throw Exit.PublishZipNoExposed

                                Details.ValidPkg _ (e :: es) _ ->
                                    return (NE.Nonempty e es)
                            )
                                |> IO.bind
                                    (\exposed ->
                                        Task.eio Exit.PublishZipBuildProblem
                                            (Build.fromExposed Reporting.silent root details Build.KeepDocs exposed)
                                            |> IO.fmap (\_ -> ())
                                    )
                        )
                )
        )



-- VERIFY VERSION


type GoodVersion
    = GoodStart
    | GoodBump V.Version M.Magnitude


verifyVersion : Env -> Pkg.Name -> V.Version -> Docs.Documentation -> Maybe Registry.KnownVersions -> Task.Task Exit.Publish ()
verifyVersion env pkg vsn newDocs publishedVersions =
    reportSemverCheck vsn <|
        case publishedVersions of
            Nothing ->
                if vsn == V.one then
                    return <| Right GoodStart

                else
                    return <| Left <| Exit.PublishNotInitialVersion vsn

            Just ((Registry.KnownVersions latest previous) as knownVersions) ->
                if vsn == latest || elem vsn previous then
                    return <| Left <| Exit.PublishAlreadyPublished vsn

                else
                    verifyBump env pkg vsn newDocs knownVersions


verifyBump : Env -> Pkg.Name -> V.Version -> Docs.Documentation -> Registry.KnownVersions -> IO (Result Exit.Publish GoodVersion)
verifyBump (Env _ cache manager _ _) pkg vsn newDocs ((Registry.KnownVersions latest _) as knownVersions) =
    case List.find (\( _, new, _ ) -> vsn == new) (Bump.getPossibilities knownVersions) of
        Nothing ->
            IO.pure <|
                Err <|
                    Exit.PublishInvalidBump vsn latest

        Just ( old, new, magnitude ) ->
            Diff.getDocs cache manager pkg old
                |> IO.fmap
                    (\result ->
                        case result of
                            Err dp ->
                                Err <| Exit.PublishCannotGetDocs old new dp

                            Ok oldDocs ->
                                let
                                    changes =
                                        Diff.diff oldDocs newDocs

                                    realNew =
                                        Diff.bump changes old
                                in
                                if new == realNew then
                                    Ok <| GoodBump old magnitude

                                else
                                    Err <|
                                        Exit.PublishBadBump old new magnitude realNew (Diff.toMagnitude changes)
                    )



-- REGISTER PACKAGES


register : Http.Manager -> Pkg.Name -> V.Version -> Docs.Documentation -> String -> Http.Sha -> Task.Task Exit.Publish ()
register manager pkg vsn docs commitHash sha =
    let
        url =
            Website.route "/register"
                [ ( "name", Pkg.toChars pkg )
                , ( "version", V.toChars vsn )
                , ( "commit-hash", commitHash )
                ]
    in
    Task.eio Exit.PublishCannotRegister <|
        Http.upload manager
            url
            [ Http.filePart "elm.json" "elm.json"
            , Http.jsonPart "docs.json" "docs.json" (Docs.jsonEncode docs)
            , Http.filePart "README.md" "README.md"
            , Http.stringPart "github-hash" (Http.shaToChars sha)
            ]



-- REPORTING


reportPublishStart : Pkg.Name -> V.Version -> Maybe Registry.KnownVersions -> Task.Task x ()
reportPublishStart pkg vsn maybeKnownVersions =
    Task.io <|
        case maybeKnownVersions of
            Nothing ->
                putStrLn <| Exit.newPackageOverview ++ "\nI will now verify that everything is in order...\n"

            Just _ ->
                putStrLn <| "Verifying " ++ Pkg.toChars pkg ++ " " ++ V.toChars vsn ++ " ...\n"



-- REPORTING PHASES


reportReadmeCheck : IO (Result x a) -> Task.Task x a
reportReadmeCheck =
    reportCheck
        "Looking for README.md"
        "Found README.md"
        "Problem with your README.md"


reportLicenseCheck : IO (Result x a) -> Task.Task x a
reportLicenseCheck =
    reportCheck
        "Looking for LICENSE"
        "Found LICENSE"
        "Problem with your LICENSE"


reportBuildCheck : IO (Result x a) -> Task.Task x a
reportBuildCheck =
    reportCheck
        "Verifying documentation..."
        "Verified documentation"
        "Problem with documentation"


reportSemverCheck : V.Version -> IO (Result x GoodVersion) -> Task.Task x ()
reportSemverCheck version work =
    let
        vsn =
            V.toChars version

        waiting =
            "Checking semantic versioning rules. Is " ++ vsn ++ " correct?"

        failure =
            "Version " ++ vsn ++ " is not correct!"

        success result =
            case result of
                GoodStart ->
                    "All packages start at version " ++ V.toChars V.one

                GoodBump oldVersion magnitude ->
                    "Version number "
                        ++ vsn
                        ++ " verified ("
                        ++ M.toChars magnitude
                        ++ " change, "
                        ++ V.toChars oldVersion
                        ++ " => "
                        ++ vsn
                        ++ ")"
    in
    void <| reportCustomCheck waiting success failure work


reportTagCheck : V.Version -> IO (Result x a) -> Task.Task x a
reportTagCheck vsn =
    reportCheck
        ("Is version " ++ V.toChars vsn ++ " tagged on GitHub?")
        ("Version " ++ V.toChars vsn ++ " is tagged on GitHub")
        ("Version " ++ V.toChars vsn ++ " is not tagged on GitHub!")


reportDownloadCheck : IO (Result x a) -> Task.Task x a
reportDownloadCheck =
    reportCheck
        "Downloading code from GitHub..."
        "Code downloaded successfully from GitHub"
        "Could not download code from GitHub!"


reportLocalChangesCheck : IO (Result x a) -> Task.Task x a
reportLocalChangesCheck =
    reportCheck
        "Checking for uncommitted changes..."
        "No uncommitted changes in local code"
        "Your local code is different than the code tagged on GitHub"


reportZipBuildCheck : IO (Result x a) -> Task.Task x a
reportZipBuildCheck =
    reportCheck
        "Verifying downloaded code..."
        "Downloaded code compiles successfully"
        "Cannot compile downloaded code!"


reportCheck : String -> String -> String -> IO (Result x a) -> Task.Task x a
reportCheck waiting success failure work =
    reportCustomCheck waiting (\_ -> success) failure work


reportCustomCheck : String -> (a -> String) -> String -> IO (Result x a) -> Task.Task x a
reportCustomCheck waiting success failure work =
    let
        putFlush doc =
            Help.toStdout doc >> IO.hFlush IO.stdout

        padded message =
            message ++ replicate (length waiting - length message) ' '
    in
    Task.eio id
        (putFlush ("  " ++ waitingMark |> D.plus (D.fromChars waiting))
            |> IO.bind
                (\_ ->
                    work
                        |> IO.bind
                            (\result ->
                                putFlush
                                    (case result of
                                        Ok a ->
                                            "\u{000D}  " ++ goodMark |> D.plus (D.fromChars (padded (success a) ++ "\n"))

                                        Err _ ->
                                            "\u{000D}  " ++ badMark |> D.plus (D.fromChars (padded failure ++ "\n\n"))
                                    )
                                    return
                                    result
                            )
                )
        )



-- MARKS


goodMark : D.Doc
goodMark =
    D.green <|
        if isWindows then
            "+"

        else
            "●"


badMark : D.Doc
badMark =
    D.red <|
        if isWindows then
            "X"

        else
            "✗"


waitingMark : D.Doc
waitingMark =
    D.dullyellow <|
        if isWindows then
            "-"

        else
            "→"


isWindows : Bool
isWindows =
    Info.os == "mingw32"

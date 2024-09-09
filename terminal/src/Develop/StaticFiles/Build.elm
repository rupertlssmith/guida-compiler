module Develop.StaticFiles.Build exposing
    ( buildReactorFrontEnd
    , readAsset
    )

import BackgroundWriter as BW
import Build
import Data.IO as IO exposing (IO)
import Data.NonEmptyList as NE
import Elm.Details as Details
import Generate
import Reporting
import Reporting.Exit as Exit
import Reporting.Task as Task
import Utils exposing (FilePath)



-- ASSETS


readAsset : FilePath -> IO String
readAsset path =
    Utils.bsReadFile ("reactor/assets/" ++ path)



-- BUILD REACTOR ELM


buildReactorFrontEnd : IO String
buildReactorFrontEnd =
    BW.withScope
        (\scope ->
            Utils.dirWithCurrentDirectory "reactor"
                (Utils.dirGetCurrentDirectory
                    |> IO.bind
                        (\root ->
                            runTaskUnsafe
                                (Task.eio Exit.ReactorBadDetails (Details.load Reporting.silent scope root)
                                    |> Task.bind
                                        (\details ->
                                            Task.eio Exit.ReactorBadBuild (Build.fromPaths Reporting.silent root details paths)
                                                |> Task.bind
                                                    (\artifacts ->
                                                        Task.mapError Exit.ReactorBadGenerate (Generate.prod root details artifacts)
                                                            |> Task.fmap (\javascript -> javascript)
                                                    )
                                        )
                                )
                        )
                )
        )


paths : NE.Nonempty FilePath
paths =
    NE.Nonempty
        "src/NotFound.elm"
        [ "src/Errors.elm"
        , "src/Index.elm"
        ]


runTaskUnsafe : Task.Task Exit.Reactor a -> IO a
runTaskUnsafe task =
    Task.run task
        |> IO.bind
            (\result ->
                case result of
                    Ok a ->
                        IO.pure a

                    Err exit ->
                        Exit.toStderr (Exit.reactorToReport exit)
                            |> IO.fmap
                                (\_ ->
                                    Utils.crash
                                        "\n--------------------------------------------------------\nError in Develop.StaticFiles.Build.buildReactorFrontEnd\nCompile with `elm make` directly to figure it out faster\n--------------------------------------------------------\n"
                                )
            )

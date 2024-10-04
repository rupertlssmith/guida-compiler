module Terminal.Develop.StaticFiles.Build exposing
    ( buildReactorFrontEnd
    , readAsset
    )

import Builder.BackgroundWriter as BW
import Builder.Build as Build
import Builder.Elm.Details as Details
import Builder.Generate as Generate
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Compiler.Data.NonEmptyList as NE
import Data.IO as IO exposing (IO)
import Utils.Crash exposing (crash)
import Utils.Main as Utils exposing (FilePath)



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
                            |> IO.fmap (\_ -> crash "\n--------------------------------------------------------\nError in Develop.StaticFiles.Build.buildReactorFrontEnd\nCompile with `elm make` directly to figure it out faster\n--------------------------------------------------------\n")
            )

module Generate exposing
    ( debug
    , dev
    , prod
    , repl
    )

import AST.Optimized as Opt
import AssocList as Dict exposing (Dict)
import Build
import Data.IO as IO exposing (IO)
import Data.Name as N
import Data.NonEmptyList as NE
import Elm.Compiler.Type.Extract as Extract
import Elm.Details as Details
import Elm.Interface as I
import Elm.ModuleName as ModuleName
import Elm.Package as Pkg
import File
import Generate.JavaScript as JS
import Generate.Mode as Mode
import Json.Decode as Decode
import Nitpick.Debug as Nitpick
import Reporting.Exit as Exit
import Reporting.Task as Task
import Stuff
import Utils exposing (FilePath, MVar)



-- NOTE: This is used by Make, Repl, and Reactor right now. But it may be
-- desireable to have Repl and Reactor to keep foreign objects in memory
-- to make things a bit faster?
-- GENERATORS


type alias Task a =
    Task.Task Exit.Generate a


debug : FilePath -> Details.Details -> Build.Artifacts -> Task String
debug root details (Build.Artifacts pkg ifaces roots modules) =
    loadObjects root details modules
        |> Task.bind
            (\loading ->
                loadTypes root ifaces modules
                    |> Task.bind
                        (\types ->
                            finalizeObjects loading
                                |> Task.fmap
                                    (\objects ->
                                        let
                                            mode =
                                                Mode.Dev (Just types)

                                            graph =
                                                objectsToGlobalGraph objects

                                            mains =
                                                gatherMains pkg objects roots
                                        in
                                        JS.generate mode graph mains
                                    )
                        )
            )


dev : FilePath -> Details.Details -> Build.Artifacts -> Task String
dev root details (Build.Artifacts pkg _ roots modules) =
    Task.bind finalizeObjects (loadObjects root details modules)
        |> Task.fmap
            (\objects ->
                let
                    mode =
                        Mode.Dev Nothing

                    graph =
                        objectsToGlobalGraph objects

                    mains =
                        gatherMains pkg objects roots
                in
                JS.generate mode graph mains
            )


prod : FilePath -> Details.Details -> Build.Artifacts -> Task String
prod root details (Build.Artifacts pkg _ roots modules) =
    Task.bind finalizeObjects (loadObjects root details modules)
        |> Task.bind
            (\objects ->
                checkForDebugUses objects
                    |> Task.fmap
                        (\_ ->
                            let
                                graph =
                                    objectsToGlobalGraph objects

                                mode =
                                    Mode.Prod (Mode.shortenFieldNames graph)

                                mains =
                                    gatherMains pkg objects roots
                            in
                            JS.generate mode graph mains
                        )
            )


repl : FilePath -> Details.Details -> Bool -> Build.ReplArtifacts -> N.Name -> Task String
repl root details ansi (Build.ReplArtifacts home modules localizer annotations) name =
    Task.bind finalizeObjects (loadObjects root details modules)
        |> Task.fmap
            (\objects ->
                let
                    graph =
                        objectsToGlobalGraph objects
                in
                JS.generateForRepl ansi localizer graph home name (Utils.find name annotations)
            )



-- CHECK FOR DEBUG


checkForDebugUses : Objects -> Task ()
checkForDebugUses (Objects _ locals) =
    case Dict.keys (Dict.filter (\_ -> Nitpick.hasDebugUses) locals) of
        [] ->
            Task.pure ()

        m :: ms ->
            Task.throw (Exit.GenerateCannotOptimizeDebugValues m ms)



-- GATHER MAINS


gatherMains : Pkg.Name -> Objects -> NE.Nonempty Build.Root -> Dict ModuleName.Canonical Opt.Main
gatherMains pkg (Objects _ locals) roots =
    Dict.fromList (List.filterMap (lookupMain pkg locals) (NE.toList roots))


lookupMain : Pkg.Name -> Dict ModuleName.Raw Opt.LocalGraph -> Build.Root -> Maybe ( ModuleName.Canonical, Opt.Main )
lookupMain pkg locals root =
    let
        toPair name (Opt.LocalGraph maybeMain _ _) =
            Maybe.map (Tuple.pair (ModuleName.Canonical pkg name)) maybeMain
    in
    case root of
        Build.Inside name ->
            Maybe.andThen (toPair name) (Dict.get name locals)

        Build.Outside name _ g ->
            toPair name g



-- LOADING OBJECTS


type LoadingObjects
    = LoadingObjects (MVar (Maybe Opt.GlobalGraph)) (Dict ModuleName.Raw (MVar (Maybe Opt.LocalGraph)))


loadObjects : FilePath -> Details.Details -> List Build.Module -> Task LoadingObjects
loadObjects root details modules =
    Task.io
        (Details.loadObjects root details
            |> IO.bind
                (\mvar ->
                    Utils.listTraverse (loadObject root) modules
                        |> IO.fmap
                            (\mvars ->
                                LoadingObjects mvar (Dict.fromList mvars)
                            )
                )
        )


loadObject : FilePath -> Build.Module -> IO ( ModuleName.Raw, MVar (Maybe Opt.LocalGraph) )
loadObject root modul =
    case modul of
        Build.Fresh name _ graph ->
            Utils.newMVar (Utils.maybeEncoder Opt.localGraphEncoder) (Just graph)
                |> IO.fmap (\mvar -> ( name, mvar ))

        Build.Cached name _ _ ->
            Utils.newEmptyMVar
                |> IO.bind
                    (\mvar ->
                        Utils.forkIO (IO.bind (Utils.putMVar (Utils.maybeEncoder Opt.localGraphEncoder) mvar) (File.readBinary Opt.localGraphDecoder (Stuff.elmo root name)))
                            |> IO.fmap (\_ -> ( name, mvar ))
                    )



-- FINALIZE OBJECTS


type Objects
    = Objects Opt.GlobalGraph (Dict ModuleName.Raw Opt.LocalGraph)


finalizeObjects : LoadingObjects -> Task Objects
finalizeObjects (LoadingObjects mvar mvars) =
    Task.eio identity
        (Utils.readMVar (Decode.maybe Opt.globalGraphDecoder) mvar
            |> IO.bind
                (\result ->
                    Utils.mapTraverse (Utils.readMVar (Decode.maybe Opt.localGraphDecoder)) mvars
                        |> IO.fmap
                            (\results ->
                                case Maybe.map2 Objects result (Utils.sequenceDictMaybe results) of
                                    Just loaded ->
                                        Ok loaded

                                    Nothing ->
                                        Err Exit.GenerateCannotLoadArtifacts
                            )
                )
        )


objectsToGlobalGraph : Objects -> Opt.GlobalGraph
objectsToGlobalGraph (Objects globals locals) =
    Dict.foldr (\_ -> Opt.addLocalGraph) globals locals



-- LOAD TYPES


loadTypes : FilePath -> Dict ModuleName.Canonical I.DependencyInterface -> List Build.Module -> Task Extract.Types
loadTypes root ifaces modules =
    Task.eio identity
        (Utils.listTraverse (loadTypesHelp root) modules
            |> IO.bind
                (\mvars ->
                    let
                        foreigns =
                            Extract.mergeMany (Dict.values (Dict.map Extract.fromDependencyInterface ifaces))
                    in
                    Utils.listTraverse (Utils.readMVar (Decode.maybe Extract.typesDecoder)) mvars
                        |> IO.fmap
                            (\results ->
                                case Utils.sequenceListMaybe results of
                                    Just ts ->
                                        Ok (Extract.merge foreigns (Extract.mergeMany ts))

                                    Nothing ->
                                        Err Exit.GenerateCannotLoadArtifacts
                            )
                )
        )


loadTypesHelp : FilePath -> Build.Module -> IO (MVar (Maybe Extract.Types))
loadTypesHelp root modul =
    case modul of
        Build.Fresh name iface _ ->
            Utils.newMVar (Utils.maybeEncoder Extract.typesEncoder) (Just (Extract.fromInterface name iface))

        Build.Cached name _ ciMVar ->
            Utils.readMVar Build.cachedInterfaceDecoder ciMVar
                |> IO.bind
                    (\cachedInterface ->
                        case cachedInterface of
                            Build.Unneeded ->
                                Utils.newEmptyMVar
                                    |> IO.bind
                                        (\mvar ->
                                            Utils.forkIO
                                                (File.readBinary I.interfaceDecoder (Stuff.elmi root name)
                                                    |> IO.bind
                                                        (\maybeIface ->
                                                            Utils.putMVar (Utils.maybeEncoder Extract.typesEncoder) mvar (Maybe.map (Extract.fromInterface name) maybeIface)
                                                        )
                                                )
                                                |> IO.fmap (\_ -> mvar)
                                        )

                            Build.Loaded iface ->
                                Utils.newMVar (Utils.maybeEncoder Extract.typesEncoder) (Just (Extract.fromInterface name iface))

                            Build.Corrupted ->
                                Utils.newMVar (Utils.maybeEncoder Extract.typesEncoder) Nothing
                    )

module Diff exposing
    ( Args(..)
    , run
    )

import BackgroundWriter as BW
import Build
import Data.IO exposing (IO)
import Data.Map as Dict
import Data.Name as Name
import Data.NonEmptyList as NE
import Deps.Diff as DD exposing (Changes(..), ModuleChanges(..), PackageChanges(..))
import Deps.Registry as Registry
import Elm.Compiler.Type as Type
import Elm.Details as Details
import Elm.Docs as Docs
import Elm.Magnitude as M
import Elm.Outline as Outline
import Elm.Package as Pkg
import Elm.Version as V
import Http
import Reporting
import Reporting.Doc as D
import Reporting.Exit as Exit
import Reporting.Exit.Help as Help
import Reporting.Render.Type.Localizer as L
import Reporting.Task as Task
import Stuff



-- RUN


type Args
    = CodeVsLatest
    | CodeVsExactly V.Version
    | LocalInquiry V.Version V.Version
    | GlobalInquiry Pkg.Name V.Version V.Version


run : Args -> () -> IO ()
run args () =
    Reporting.attempt Exit.diffToReport
        (Task.run
            (getEnv
                |> Task.bind (\env -> diff env args)
            )
        )



-- ENVIRONMENT


type Env
    = Env (Maybe String) Stuff.PackageCache Http.Manager Registry.Registry


getEnv : Task Env
getEnv =
    Task.io Stuff.findRoot
        |> Task.bind
            (\maybeRoot ->
                Task.io Stuff.getPackageCache
                    |> Task.bind
                        (\cache ->
                            Task.io Http.getManager
                                |> Task.bind
                                    (\manager ->
                                        Task.eio Exit.DiffMustHaveLatestRegistry (Registry.latest manager cache)
                                            |> Task.fmap (\registry -> Env maybeRoot cache manager registry)
                                    )
                        )
            )



-- DIFF


type alias Task a =
    Task.Task Exit.Diff a


diff : Env -> Args -> Task ()
diff ((Env _ _ _ registry) as env) args =
    case args of
        GlobalInquiry name v1 v2 ->
            case Registry.getVersions_ name registry of
                Ok vsns ->
                    getDocs env name vsns (V.min v1 v2)
                        |> Task.bind
                            (\oldDocs ->
                                getDocs env name vsns (V.max v1 v2)
                                    |> Task.bind (\newDocs -> writeDiff oldDocs newDocs)
                            )

                Err suggestions ->
                    Task.throw <| Exit.DiffUnknownPackage name suggestions

        LocalInquiry v1 v2 ->
            readOutline env
                |> Task.bind
                    (\( name, vsns ) ->
                        getDocs env name vsns (V.min v1 v2)
                            |> Task.bind
                                (\oldDocs ->
                                    getDocs env name vsns (V.max v1 v2)
                                        |> Task.bind (\newDocs -> writeDiff oldDocs newDocs)
                                )
                    )

        CodeVsLatest ->
            readOutline env
                |> Task.bind
                    (\( name, vsns ) ->
                        getLatestDocs env name vsns
                            |> Task.bind
                                (\oldDocs ->
                                    generateDocs env
                                        |> Task.bind (\newDocs -> writeDiff oldDocs newDocs)
                                )
                    )

        CodeVsExactly version ->
            readOutline env
                |> Task.bind
                    (\( name, vsns ) ->
                        getDocs env name vsns version
                            |> Task.bind
                                (\oldDocs ->
                                    generateDocs env
                                        |> Task.bind (\newDocs -> writeDiff oldDocs newDocs)
                                )
                    )



-- GET DOCS


getDocs : Env -> Pkg.Name -> Registry.KnownVersions -> V.Version -> Task Docs.Documentation
getDocs (Env _ cache manager _) name (Registry.KnownVersions latest previous) version =
    if latest == version || List.member version previous then
        Task.eio (Exit.DiffDocsProblem version) <| DD.getDocs cache manager name version

    else
        Task.throw <| Exit.DiffUnknownVersion name version (latest :: previous)


getLatestDocs : Env -> Pkg.Name -> Registry.KnownVersions -> Task Docs.Documentation
getLatestDocs (Env _ cache manager _) name (Registry.KnownVersions latest _) =
    Task.eio (Exit.DiffDocsProblem latest) <| DD.getDocs cache manager name latest



-- READ OUTLINE


readOutline : Env -> Task ( Pkg.Name, Registry.KnownVersions )
readOutline (Env maybeRoot _ _ registry) =
    case maybeRoot of
        Nothing ->
            Task.throw <| Exit.DiffNoOutline

        Just root ->
            Task.io (Outline.read root)
                |> Task.bind
                    (\result ->
                        case result of
                            Err err ->
                                Task.throw <| Exit.DiffBadOutline err

                            Ok outline ->
                                case outline of
                                    Outline.App _ ->
                                        Task.throw <| Exit.DiffApplication

                                    Outline.Pkg (Outline.PkgOutline pkg _ _ _ _ _ _ _) ->
                                        case Registry.getVersions pkg registry of
                                            Just vsns ->
                                                Task.pure ( pkg, vsns )

                                            Nothing ->
                                                Task.throw Exit.DiffUnpublished
                    )



-- GENERATE DOCS


generateDocs : Env -> Task Docs.Documentation
generateDocs (Env maybeRoot _ _ _) =
    case maybeRoot of
        Nothing ->
            Task.throw <| Exit.DiffNoOutline

        Just root ->
            Task.eio Exit.DiffBadDetails
                (BW.withScope (\scope -> Details.load Reporting.silent scope root))
                |> Task.bind
                    (\details ->
                        case Details.outline_ details of
                            Details.ValidApp _ ->
                                Task.throw Exit.DiffApplication

                            Details.ValidPkg _ exposed _ ->
                                case exposed of
                                    [] ->
                                        Task.throw Exit.DiffNoExposed

                                    e :: es ->
                                        Task.eio Exit.DiffBadBuild <|
                                            Build.fromExposed Reporting.silent root details Build.KeepDocs (NE.Nonempty e es)
                    )



-- WRITE DIFF


writeDiff : Docs.Documentation -> Docs.Documentation -> Task ()
writeDiff oldDocs newDocs =
    let
        changes =
            DD.diff oldDocs newDocs

        localizer =
            L.fromNames (Dict.union oldDocs newDocs)
    in
    Task.io (Help.toStdout (toDoc localizer changes ++ "\n"))



-- TO DOC


toDoc : L.Localizer -> PackageChanges -> D.Doc
toDoc localizer ((PackageChanges added changed removed) as changes) =
    if null added && Map.null changed && null removed then
        D.fromChars "No API changes detected, so this is a"
            |> D.plus (D.green (D.fromChars "PATCH"))
            |> D.plus (D.fromChars "change.")

    else
        let
            magDoc =
                D.fromChars (M.toChars (DD.toMagnitude changes))

            header =
                D.fromChars "This is a"
                    |> D.plus (D.green magDoc)
                    |> D.plus (D.fromChars "change.")

            addedChunk =
                if null added then
                    []

                else
                    [ Chunk "ADDED MODULES" M.MINOR <|
                        D.vcat <|
                            map D.fromName added
                    ]

            removedChunk =
                if null removed then
                    []

                else
                    [ Chunk "REMOVED MODULES" M.MAJOR <|
                        D.vcat <|
                            map D.fromName removed
                    ]

            chunks =
                addedChunk ++ removedChunk ++ map (changesToChunk localizer) (Map.toList changed)
        in
        D.vcat (header :: "" :: map chunkToDoc chunks)


type Chunk
    = Chunk String M.Magnitude D.Doc


chunkToDoc : Chunk -> D.Doc
chunkToDoc (Chunk title magnitude details) =
    let
        header =
            D.fromChars "----"
                |> D.plus (D.fromChars title)
                |> D.plus (D.fromChars "-")
                |> D.plus (D.fromChars (M.toChars magnitude))
                |> D.plus (D.fromChars "----")
    in
    D.vcat
        [ D.dullcyan header
        , D.fromChars ""
        , D.indent 4 details
        , D.fromChars ""
        , D.fromChars ""
        ]


changesToChunk : L.Localizer -> ( Name.Name, ModuleChanges ) -> Chunk
changesToChunk localizer ( name, (ModuleChanges unions aliases values binops) as changes ) =
    let
        magnitude =
            DD.moduleChangeMagnitude changes

        ( unionAdd, unionChange, unionRemove ) =
            changesToDocTriple (unionToDoc localizer) unions

        ( aliasAdd, aliasChange, aliasRemove ) =
            changesToDocTriple (aliasToDoc localizer) aliases

        ( valueAdd, valueChange, valueRemove ) =
            changesToDocTriple (valueToDoc localizer) values

        ( binopAdd, binopChange, binopRemove ) =
            changesToDocTriple (binopToDoc localizer) binops
    in
    Chunk (Name.toChars name) magnitude <|
        D.vcat <|
            List.intersperse "" <|
                Maybe.catMaybes <|
                    [ changesToDoc "Added" unionAdd aliasAdd valueAdd binopAdd
                    , changesToDoc "Removed" unionRemove aliasRemove valueRemove binopRemove
                    , changesToDoc "Changed" unionChange aliasChange valueChange binopChange
                    ]


changesToDocTriple : (k -> v -> D.Doc) -> Changes k v -> ( List D.Doc, List D.Doc, List D.Doc )
changesToDocTriple entryToDoc (Changes added changed removed) =
    let
        indented ( name, value ) =
            D.indent 4 (entryToDoc name value)

        diffed ( name, ( oldValue, newValue ) ) =
            D.vcat
                [ "  - " ++ entryToDoc name oldValue
                , "  + " ++ entryToDoc name newValue
                , ""
                ]
    in
    ( map indented (Map.toList added)
    , map diffed (Map.toList changed)
    , map indented (Map.toList removed)
    )


changesToDoc : String -> List D.Doc -> List D.Doc -> List D.Doc -> List D.Doc -> Maybe D.Doc
changesToDoc categoryName unions aliases values binops =
    if null unions && null aliases && null values && null binops then
        Nothing

    else
        Just <|
            D.vcat <|
                D.fromChars categoryName
                    ++ ":"
                    :: unions
                    ++ aliases
                    ++ binops
                    ++ values


unionToDoc : L.Localizer -> Name.Name -> Docs.Union -> D.Doc
unionToDoc localizer name (Docs.Union _ tvars ctors) =
    let
        setup =
            D.fromChars "type"
                |> D.plus (D.fromName name)
                |> D.plus (D.hsep (List.map D.fromName tvars))

        ctorDoc ( ctor, tipes ) =
            typeDoc localizer (Type.Type ctor tipes)
    in
    D.hang 4 (D.sep (setup :: zipWith (++) ("=" :: repeat "|") (map ctorDoc ctors)))


aliasToDoc : L.Localizer -> Name.Name -> Docs.Alias -> D.Doc
aliasToDoc localizer name (Docs.Alias _ tvars tipe) =
    let
        declaration =
            "type" ++ "alias" ++ D.hsep (map D.fromName (name :: tvars)) ++ "="
    in
    D.hang 4 (D.sep [ declaration, typeDoc localizer tipe ])


valueToDoc : L.Localizer -> Name.Name -> Docs.Value -> D.Doc
valueToDoc localizer name (Docs.Value _ tipe) =
    D.hang 4 <| D.sep [ D.fromName name ++ ":", typeDoc localizer tipe ]


binopToDoc : L.Localizer -> Name.Name -> Docs.Binop -> D.Doc
binopToDoc localizer name (Docs.Binop _ tipe associativity (Docs.Precedence n)) =
    let
        details =
            "    (" ++ D.fromName assoc ++ "/" ++ D.fromInt n ++ ")"

        assoc =
            case associativity of
                Docs.Left ->
                    "left"

                Docs.Non ->
                    "non"

                Docs.Right ->
                    "right"
    in
    "(" ++ D.fromName name ++ ")" ++ ":" ++ typeDoc localizer tipe ++ D.black details


typeDoc : L.Localizer -> Type.Type -> D.Doc
typeDoc localizer tipe =
    Type.toDoc localizer Type.None tipe

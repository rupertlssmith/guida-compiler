module Compile exposing
    ( Artifacts(..)
    , compile
    )

import AST.Canonical as Can
import AST.Optimized as Opt
import AST.Source as Src
import AssocList exposing (Dict)
import Canonicalize.Module as Canonicalize
import Data.IO as IO exposing (IO)
import Data.Name as Name exposing (Name)
import Elm.Interface as I
import Elm.ModuleName as ModuleName
import Elm.Package as Pkg
import Nitpick.PatternMatches as PatternMatches
import Optimize.Module as Optimize
import Reporting.Error as E
import Reporting.Render.Type.Localizer as Localizer
import Reporting.Result as R
import Type.Constrain.Module as Type
import Type.Solve as Type



-- COMPILE


type Artifacts
    = Artifacts Can.Module (Dict Name Can.Annotation) Opt.LocalGraph


compile : Pkg.Name -> Dict ModuleName.Raw I.Interface -> Src.Module -> IO (Result E.Error Artifacts)
compile pkg ifaces modul =
    IO.pure (canonicalize pkg ifaces modul)
        |> IO.bind
            (\canonicalResult ->
                case canonicalResult of
                    Ok canonical ->
                        typeCheck modul canonical
                            |> IO.fmap
                                (Result.map2 (\() annotations -> annotations) (nitpick canonical)
                                    >> Result.andThen
                                        (\annotations ->
                                            optimize modul annotations canonical
                                                |> Result.map (\objects -> Artifacts canonical annotations objects)
                                        )
                                )

                    Err err ->
                        IO.pure (Err err)
            )



-- PHASES


canonicalize : Pkg.Name -> Dict ModuleName.Raw I.Interface -> Src.Module -> Result E.Error Can.Module
canonicalize pkg ifaces modul =
    case Tuple.second (R.run (Canonicalize.canonicalize pkg ifaces modul)) of
        Ok canonical ->
            Ok canonical

        Err errors ->
            Err (E.BadNames errors)


typeCheck : Src.Module -> Can.Module -> IO (Result E.Error (Dict Name Can.Annotation))
typeCheck modul canonical =
    IO.bind Type.run (Type.constrain canonical)
        |> IO.fmap
            (\result ->
                case result of
                    Ok annotations ->
                        Ok annotations

                    Err errors ->
                        Err (E.BadTypes (Localizer.fromModule modul) errors)
            )


nitpick : Can.Module -> Result E.Error ()
nitpick canonical =
    case PatternMatches.check canonical of
        Ok () ->
            Ok ()

        Err errors ->
            Err (E.BadPatterns errors)


optimize : Src.Module -> Dict Name.Name Can.Annotation -> Can.Module -> Result E.Error Opt.LocalGraph
optimize modul annotations canonical =
    case Tuple.second (R.run (Optimize.optimize annotations canonical)) of
        Ok localGraph ->
            Ok localGraph

        Err errors ->
            Err (E.BadMains (Localizer.fromModule modul) errors)

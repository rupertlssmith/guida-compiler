module Deps.Website exposing
    ( domain
    , metadata
    , route
    )

import Elm.Package as Pkg
import Elm.Version as V
import Http


domain : String
domain =
    "https://package.elm-lang.org"


route : String -> List ( String, String ) -> String
route path params =
    Http.toUrl (domain ++ path) params


metadata : Pkg.Name -> V.Version -> String -> String
metadata name version file =
    domain ++ "/packages/" ++ Pkg.toUrl name ++ "/" ++ V.toChars version ++ "/" ++ file

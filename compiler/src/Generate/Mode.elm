module Generate.Mode exposing
    ( Mode(..)
    , ShortFieldNames
    , isDebug
    , shortenFieldNames
    )

import AST.Optimized as Opt
import AssocList as Dict exposing (Dict)
import Data.Name as Name
import Elm.Compiler.Type.Extract as Extract
import Generate.JavaScript.Name as JsName



-- MODE


type Mode
    = Dev (Maybe Extract.Types)
    | Prod ShortFieldNames


isDebug : Mode -> Bool
isDebug mode =
    case mode of
        Dev (Just _) ->
            True

        Dev Nothing ->
            False

        Prod _ ->
            False



-- SHORTEN FIELD NAMES


type alias ShortFieldNames =
    Dict Name.Name JsName.Name


shortenFieldNames : Opt.GlobalGraph -> ShortFieldNames
shortenFieldNames (Opt.GlobalGraph _ frequencies) =
    Dict.foldr (\_ -> addToShortNames) Dict.empty <|
        Dict.foldr addToBuckets Dict.empty frequencies


addToBuckets : Name.Name -> Int -> Dict Int (List Name.Name) -> Dict Int (List Name.Name)
addToBuckets field frequency buckets =
    Dict.update frequency
        (Maybe.map ((::) field)
            >> Maybe.withDefault [ field ]
            >> Just
        )
        buckets


addToShortNames : List Name.Name -> ShortFieldNames -> ShortFieldNames
addToShortNames fields shortNames =
    List.foldl addField shortNames fields


addField : Name.Name -> ShortFieldNames -> ShortFieldNames
addField field shortNames =
    let
        rename =
            JsName.fromInt (Dict.size shortNames)
    in
    Dict.insert field rename shortNames

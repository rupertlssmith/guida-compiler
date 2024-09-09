module Canonicalize.Environment.Dups exposing
    ( Tracker
    , checkFields
    , checkFields_
    , detect
    , insert
    , none
    , one
    , union
    , unions
    )

import AssocList as Dict exposing (Dict)
import Data.Name exposing (Name)
import Data.OneOrMore as OneOrMore exposing (OneOrMore)
import Reporting.Annotation as A
import Reporting.Error.Canonicalize as Error exposing (Error)
import Reporting.Result as R
import Utils



-- DUPLICATE TRACKER


type alias Tracker value =
    Dict Name (OneOrMore (Info value))


type alias Info value =
    { region : A.Region
    , value : value
    }



-- DETECT


type alias ToError =
    Name -> A.Region -> A.Region -> Error


detect : ToError -> Tracker a -> R.RResult i w Error (Dict Name a)
detect toError dict =
    Dict.foldl
        (\name values ->
            R.bind
                (\acc ->
                    R.fmap (\b -> Dict.insert name b acc)
                        (detectHelp toError name values)
                )
        )
        (R.ok Dict.empty)
        dict


detectHelp : ToError -> Name -> OneOrMore (Info a) -> R.RResult i w Error a
detectHelp toError name values =
    case values of
        OneOrMore.One { value } ->
            R.ok value

        OneOrMore.More left right ->
            let
                ( info1, info2 ) =
                    OneOrMore.getFirstTwo left right
            in
            R.throw (toError name info1.region info2.region)



-- CHECK FIELDS


checkFields : List ( A.Located Name, a ) -> R.RResult i w Error (Dict Name a)
checkFields fields =
    detect Error.DuplicateField (List.foldr addField none fields)


addField : ( A.Located Name, a ) -> Tracker a -> Tracker a
addField ( A.At region name, value ) dups =
    Utils.mapInsertWith OneOrMore.more name (OneOrMore.one (Info region value)) dups


checkFields_ : (A.Region -> a -> b) -> List ( A.Located Name, a ) -> R.RResult i w Error (Dict Name b)
checkFields_ toValue fields =
    detect Error.DuplicateField (List.foldr (addField_ toValue) none fields)


addField_ : (A.Region -> a -> b) -> ( A.Located Name, a ) -> Tracker b -> Tracker b
addField_ toValue ( A.At region name, value ) dups =
    Utils.mapInsertWith OneOrMore.more name (OneOrMore.one (Info region (toValue region value))) dups



-- BUILDING DICTIONARIES


none : Tracker a
none =
    Dict.empty


one : Name -> A.Region -> value -> Tracker value
one name region value =
    Dict.singleton name (OneOrMore.one (Info region value))


insert : Name -> A.Region -> a -> Tracker a -> Tracker a
insert name region value dict =
    Utils.mapInsertWith (\new old -> OneOrMore.more old new) name (OneOrMore.one (Info region value)) dict


union : Tracker a -> Tracker a -> Tracker a
union a b =
    Utils.mapUnionWith OneOrMore.more a b


unions : List (Tracker a) -> Tracker a
unions dicts =
    List.foldl union Dict.empty dicts

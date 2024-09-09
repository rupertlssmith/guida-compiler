module Data.Map.Utils exposing
    ( any
    , fromKeys
    , fromValues
    )

import AssocList as Dict exposing (Dict)



-- FROM KEYS


fromKeys : (comparable -> v) -> List comparable -> Dict comparable v
fromKeys toValue keys =
    Dict.fromList (List.map (\k -> ( k, toValue k )) keys)


fromValues : (v -> comparable) -> List v -> Dict comparable v
fromValues toKey values =
    Dict.fromList (List.map (\v -> ( toKey v, v )) values)



-- ANY


any : (v -> Bool) -> Dict comparable v -> Bool
any isGood dict =
    Dict.foldl (\_ v acc -> isGood v || acc) False dict

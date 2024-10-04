module Compiler.Data.Map.Utils exposing
    ( any
    , fromKeys
    , fromValues
    )

import Data.Map as Dict exposing (Dict)



-- FROM KEYS


fromKeys : (comparable -> v) -> List comparable -> Dict comparable v
fromKeys toValue keys =
    Dict.fromList compare (List.map (\k -> ( k, toValue k )) keys)


fromValues : (v -> comparable) -> List v -> Dict comparable v
fromValues toKey values =
    Dict.fromList compare (List.map (\v -> ( toKey v, v )) values)



-- ANY


any : (v -> Bool) -> Dict comparable v -> Bool
any isGood dict =
    Dict.foldl (\_ v acc -> isGood v || acc) False dict

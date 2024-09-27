module Data.Maybe exposing (maybe)


maybe : b -> (a -> b) -> Maybe a -> b
maybe defaultValue f =
    Maybe.map f
        >> Maybe.withDefault defaultValue

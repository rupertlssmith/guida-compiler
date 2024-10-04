module Compiler.Data.NonEmptyList exposing
    ( Nonempty(..)
    , cons
    , foldl
    , foldl1
    , foldr
    , map
    , singleton
    , sortBy
    , toList
    )

-- LIST


type Nonempty a
    = Nonempty a (List a)


singleton : a -> Nonempty a
singleton a =
    Nonempty a []


cons : a -> Nonempty a -> Nonempty a
cons a (Nonempty b bs) =
    Nonempty b (bs ++ [ a ])


toList : Nonempty a -> List a
toList (Nonempty x xs) =
    x :: xs



-- INSTANCES


map : (a -> b) -> Nonempty a -> Nonempty b
map func (Nonempty x xs) =
    Nonempty (func x) (List.map func xs)


foldr : (a -> b -> b) -> b -> Nonempty a -> b
foldr step state (Nonempty x xs) =
    List.foldr step state (x :: xs)


foldl : (a -> b -> b) -> b -> Nonempty a -> b
foldl step state (Nonempty x xs) =
    List.foldl step state (x :: xs)


foldl1 : (a -> a -> a) -> Nonempty a -> a
foldl1 step (Nonempty x xs) =
    List.foldl step x xs



-- SORT BY


sortBy : (a -> comparable) -> Nonempty a -> Nonempty a
sortBy toRank (Nonempty x xs) =
    let
        comparison a b =
            compare (toRank a) (toRank b)
    in
    case List.sortWith comparison xs of
        [] ->
            Nonempty x []

        y :: ys ->
            case comparison x y of
                LT ->
                    Nonempty x (y :: ys)

                EQ ->
                    Nonempty x (y :: ys)

                GT ->
                    Nonempty y (List.sortWith comparison (x :: ys))

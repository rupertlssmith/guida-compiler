module Compiler.Data.Bag exposing
    ( Bag(..)
    , append
    , empty
    , fromList
    , map
    , one
    , toList
    )

-- BAGS


type Bag a
    = Empty
    | One a
    | Two (Bag a) (Bag a)



-- HELPERS


empty : Bag a
empty =
    Empty


one : a -> Bag a
one =
    One


append : Bag a -> Bag a -> Bag a
append left right =
    case ( left, right ) of
        ( other, Empty ) ->
            other

        ( Empty, other ) ->
            other

        _ ->
            Two left right



-- MAP


map : (a -> b) -> Bag a -> Bag b
map func bag =
    case bag of
        Empty ->
            Empty

        One a ->
            One (func a)

        Two left right ->
            Two (map func left) (map func right)



-- TO LIST


toList : Bag a -> List a
toList bag =
    toListHelp bag []


toListHelp : Bag a -> List a -> List a
toListHelp bag list =
    case bag of
        Empty ->
            list

        One x ->
            x :: list

        Two a b ->
            toListHelp a (toListHelp b list)



-- FROM LIST


fromList : (a -> b) -> List a -> Bag b
fromList func list =
    case list of
        [] ->
            Empty

        first :: rest ->
            List.foldl (add func) (One (func first)) rest


add : (a -> b) -> a -> Bag b -> Bag b
add func value bag =
    Two (One (func value)) bag

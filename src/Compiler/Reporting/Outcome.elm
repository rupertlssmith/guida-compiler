module Compiler.Reporting.Outcome exposing
    ( Outcome(..)
    , mapError
    , ok
    , run
    , throw
    , warn
    )

import Compiler.Data.OneOrMore as OneOrMore exposing (OneOrMore)
import Compiler.Reporting.Warning exposing (Warning)



-- RESULT


type Outcome info warnings error a result
    = Outcome
        (info
         -> warnings
         -> (info -> warnings -> OneOrMore error -> result)
         -> (info -> warnings -> a -> result)
         -> result
        )


run : Outcome () (List w) e a ( List w, Result (OneOrMore e) a ) -> ( List w, Result (OneOrMore e) a )
run (Outcome k) =
    k ()
        []
        (\() w e -> ( List.reverse w, Err e ))
        (\() w a -> ( List.reverse w, Ok a ))



-- HELPERS


ok : a -> Outcome i w e a r
ok a =
    Outcome
        (\i w _ good ->
            good i w a
        )


warn : Warning -> Outcome i (List Warning) e () r
warn warning =
    Outcome
        (\i warnings _ good ->
            good i (warning :: warnings) ()
        )


throw : e -> Outcome i w e a r
throw e =
    Outcome
        (\i w bad _ ->
            bad i w (OneOrMore.one e)
        )


mapError : (e -> e_) -> Outcome i w e a r -> Outcome i w e_ a r
mapError func (Outcome k) =
    Outcome
        (\i w bad good ->
            let
                bad1 i1 w1 e1 =
                    bad i1 w1 (OneOrMore.map func e1)
            in
            k i w bad1 good
        )

module Compiler.Parse.Number exposing
    ( Number(..)
    , number
    , precedence
    )

import Compiler.AST.Utils.Binop as Binop
import Compiler.Parse.NewPrimitives as P


type Number
    = Int Int
    | Float Float


number : P.Parser Number
number =
    P.number
        { int = Ok Int
        , hex = Ok Int
        , octal = Err (P.Problem_Number P.NP_End)
        , binary = Err (P.Problem_Number P.NP_End)
        , float = Ok Float
        , invalid = P.Problem_Number P.NP_End
        , expecting = P.Problem_Number P.NP_End
        }


precedence : P.Parser Binop.Precedence
precedence =
    P.getChompedString (P.chompIf Char.isDigit (P.Problem_Number P.NP_End))
        |> P.andThen (\s ->
            case String.toInt s of
                Just n ->
                    if 0 <= n && n <= 9 then
                        P.succeed n
                    else
                        P.problem (P.Problem_Number P.NP_End)

                Nothing ->
                    P.problem (P.Problem_Number P.NP_End)
        )

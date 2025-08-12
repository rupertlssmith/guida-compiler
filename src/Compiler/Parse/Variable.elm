module Compiler.Parse.Variable exposing
    ( Upper(..)
    , chompInnerChars
    , chompLower
    , chompUpper
    , foreignAlpha
    , foreignUpper
    , getInnerWidth
    , getInnerWidthHelp
    , getUpperWidth
    , isDot
    , isReservedWord
    , lower
    , moduleName
    , upper
    )

import Bitwise
import Compiler.AST.Source as Src
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Parse.NewPrimitives as P
import Data.Set as EverySet exposing (EverySet)



-- LOCAL UPPER


upper : P.Problem -> P.Parser Name
upper problem =
    P.variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = EverySet.fromList identity []
        , expecting = problem
        }



-- LOCAL LOWER


lower : P.Problem -> P.Parser Name
lower problem =
    P.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = reservedWords
        , expecting = problem
        }


isReservedWord : Name.Name -> Bool
isReservedWord name =
    EverySet.member identity name reservedWords


reservedWords : EverySet String Name
reservedWords =
    EverySet.fromList identity
        [ "if"
        , "then"
        , "else"
        , "case"
        , "of"
        , "let"
        , "in"
        , "type"
        , "module"
        , "where"
        , "import"
        , "exposing"
        , "as"
        , "port"
        ]



-- MODULE NAME


moduleName : P.Problem -> P.Parser Name
moduleName problem =
    P.succeed (\h t -> h ++ String.join "." t)
        |= upper problem
        |= P.loop [] (\revParts ->
            P.oneOf
                [ P.succeed ()
                    |. P.word1 '.' problem
                    |> P.andThen (\_ ->
                        upper problem
                            |> P.andThen (\part -> P.succeed (P.Loop (part :: revParts)))
                      )
                , P.succeed (P.Done (List.reverse revParts))
                ]
           )



-- FOREIGN UPPER


type Upper
    = Unqualified Name
    | Qualified Name Name


foreignUpper : (Row -> Col -> x) -> P.Parser x Upper
foreignUpper toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                ( upperStart, upperEnd, newCol ) =
                    foreignUpperHelp src pos end col
            in
            if upperStart == upperEnd then
                P.Eerr row newCol toError

            else
                let
                    newState : P.State
                    newState =
                        P.State src upperEnd end indent row newCol

                    name : Name
                    name =
                        Name.fromPtr src upperStart upperEnd

                    upperName : Upper
                    upperName =
                        if upperStart == pos then
                            Unqualified name

                        else
                            let
                                home : Name
                                home =
                                    Name.fromPtr src pos (upperStart + -1)
                            in
                            Qualified home name
                in
                P.Cok upperName newState


foreignUpperHelp : String -> Int -> Int -> Col -> ( Int, Int, Col )
foreignUpperHelp src pos end col =
    let
        ( newPos, newCol ) =
            chompUpper src pos end col
    in
    if pos == newPos then
        ( pos, pos, col )

    else if isDot src newPos end then
        foreignUpperHelp src (newPos + 1) end (newCol + 1)

    else
        ( pos, newPos, newCol )



-- FOREIGN ALPHA


foreignAlpha : (Row -> Col -> x) -> P.Parser x Src.Expr_
foreignAlpha toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                ( ( alphaStart, alphaEnd ), ( newCol, varType ) ) =
                    foreignAlphaHelp src pos end col
            in
            if alphaStart == alphaEnd then
                P.Eerr row newCol toError

            else
                let
                    name : Name
                    name =
                        Name.fromPtr src alphaStart alphaEnd

                    newState : P.State
                    newState =
                        P.State src alphaEnd end indent row newCol
                in
                if alphaStart == pos then
                    if isReservedWord name then
                        P.Eerr row col toError

                    else
                        P.Cok (Src.Var varType name) newState

                else
                    let
                        home : Name
                        home =
                            Name.fromPtr src pos (alphaStart + -1)
                    in
                    P.Cok (Src.VarQual varType home name) newState


foreignAlphaHelp : String -> Int -> Int -> Col -> ( ( Int, Int ), ( Col, Src.VarType ) )
foreignAlphaHelp src pos end col =
    let
        ( lowerPos, lowerCol ) =
            chompLower src pos end col
    in
    if pos < lowerPos then
        ( ( pos, lowerPos ), ( lowerCol, Src.LowVar ) )

    else
        let
            ( upperPos, upperCol ) =
                chompUpper src pos end col
        in
        if pos == upperPos then
            ( ( pos, pos ), ( col, Src.CapVar ) )

        else if isDot src upperPos end then
            foreignAlphaHelp src (upperPos + 1) end (upperCol + 1)

        else
            ( ( pos, upperPos ), ( upperCol, Src.CapVar ) )



---- CHAR CHOMPERS ----
-- DOTS


isDot : String -> Int -> Int -> Bool
isDot src pos end =
    pos < end && P.unsafeIndex src pos == '.'



-- UPPER CHARS





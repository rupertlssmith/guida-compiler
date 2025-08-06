module Compiler.Parse.Variable exposing
    ( Upper(..)
    , foreignAlpha
    , foreignUpper
    , isReservedWord
    , lower
    , moduleName
    , upper
    )

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Parse.NewPrimitives as P
import Data.Set as Set
import Parser exposing (..)
import Parser.Advanced as Advanced



-- LOCAL UPPER


upper : (P.Row -> P.Col -> x) -> P.Parser x Name
upper toError =
    let
        upperVar =
            Advanced.variable
                { start = Char.isUpper
                , inner = \c -> Char.isAlphaNum c || c == '_'
                , reserved = Set.empty
                }
                (\_ r c -> toError r c)
    in
    map Name.fromChars upperVar



-- LOCAL LOWER


lower : (P.Row -> P.Col -> x) -> P.Parser x Name
lower toError =
    let
        lowerVar =
            Advanced.variable
                { start = Char.isLower
                , inner = \c -> Char.isAlphaNum c || c == '_'
                , reserved = reservedWords
                }
                (\_ r c -> toError r c)
    in
    map Name.fromChars lowerVar


isReservedWord : Name.Name -> Bool
isReservedWord name =
    Set.member name reservedWords


reservedWords : Set.Set String
reservedWords =
    Set.fromList
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


moduleName : (P.Row -> P.Col -> x) -> P.Parser x Name
moduleName toError =
    let
        part =
            Advanced.variable
                { start = Char.isUpper
                , inner = \c -> Char.isAlphaNum c || c == '_'
                , reserved = Set.empty
                }
                (\_ r c -> toError r c)
    in
    map (String.join "." >> Name.fromChars)
        (sequence
            { start = part
            , separator = "."
            , end = \_ r c -> toError r c
            , spaces = succeed ()
            , item = part
            }
        )



-- FOREIGN UPPER


type Upper
    = Unqualified Name
    | Qualified Name Name


foreignUpper : (P.Row -> P.Col -> x) -> P.Parser x Upper
foreignUpper toError =
    let
        part =
            Advanced.variable
                { start = Char.isUpper
                , inner = \c -> Char.isAlphaNum c || c == '_'
                , reserved = Set.empty
                }
                (\_ r c -> toError r c)
                |> map Name.fromChars

        qualified =
            succeed Qualified
                |> andThen (\f -> map f part)
                |> andThen (\ctor -> symbol "." |> andThen (\_ -> map ctor part))

        unqualified =
            map Unqualified part
    in
    oneOf [ qualified, unqualified ]



-- FOREIGN ALPHA


foreignAlpha : (P.Row -> P.Col -> x) -> P.Parser x Src.Expr_
foreignAlpha toError =
    let
        anyVar =
            Advanced.variable
                { start = Char.isAlpha
                , inner = \c -> Char.isAlphaNum c || c == '_'
                , reserved = Set.empty
                }
                (\_ r c -> toError r c)

        parts =
            sequence
                { start = anyVar
                , separator = "."
                , end = \_ r c -> toError r c
                , spaces = succeed ()
                , item = anyVar
                }
    in
    parts
        |> andThen
            (\rawParts ->
                let
                    allParts =
                        rawParts

                    lastPart =
                        List.head (List.reverse allParts) |> Maybe.withDefault ""

                    moduleParts =
                        List.take (List.length allParts - 1) allParts
                in
                if List.all (\p -> Char.isUpper (String.head p |> Maybe.withDefault ' ')) moduleParts then
                    let
                        home =
                            String.join "." moduleParts

                        name =
                            lastPart

                        varType =
                            if Char.isUpper (String.head name |> Maybe.withDefault ' ') then
                                Src.CapVar

                            else
                                Src.LowVar
                    in
                    if isReservedWord name then
                        Advanced.problem toError

                    else if home == "" then
                        succeed (Src.Var varType (Name.fromChars name))

                    else
                        succeed (Src.VarQual varType (Name.fromChars home) (Name.fromChars name))

                else
                    Advanced.problem toError
            )

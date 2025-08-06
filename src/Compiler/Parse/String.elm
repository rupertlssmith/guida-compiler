module Compiler.Parse.String exposing
    ( character
    , string
    )

import Compiler.Parse.NewPrimitives as P
import Compiler.Reporting.Error.Syntax as E
import Parser exposing (..)
import Parser.Advanced as Advanced



-- PUBLIC


character : (P.Row -> P.Col -> x) -> (E.Char -> P.Row -> P.Col -> x) -> P.Parser x String
character toExpectation toError =
    let
        charParser =
            Advanced.symbol "'" (\_ r c -> toExpectation r c)
                |> andThen
                    (\_ ->
                        loop [] (charContentHelp toError)
                            |> andThen
                                (\chars ->
                                    case chars of
                                        [ c ] ->
                                            succeed (String.fromChar c)

                                        _ ->
                                            Advanced.problem (toError (E.CharNotString (List.length chars)))
                                )
                    )
                |> andThen (\s -> Advanced.symbol "'" (\_ r c -> toError E.CharEndless r c) |> andThen (\_ -> succeed s))
    in
    charParser


string : (P.Row -> P.Col -> x) -> (E.String_ -> P.Row -> P.Col -> x) -> P.Parser x String
string toExpectation toError =
    oneOf
        [ multiLineString toError
        , singleLineString toExpectation toError
        ]



-- STRINGS


singleLineString : (P.Row -> P.Col -> x) -> (E.String_ -> P.Row -> P.Col -> x) -> P.Parser x String
singleLineString toExpectation toError =
    Advanced.symbol "\"" (\_ r c -> toExpectation r c)
        |> andThen
            (\_ ->
                loop [] (stringContentHelp "\"" (\e -> toError (E.StringEscape e)) (toError E.StringEndless_Single))
            )


multiLineString : (E.String_ -> P.Row -> P.Col -> x) -> P.Parser x String
multiLineString toError =
    Advanced.symbol "\"\"\"" (\_ r c -> toError E.StringEndless_Multi r c)
        |> andThen
            (\_ ->
                loop [] (stringContentHelp "\"\"\"" (\e -> toError (E.StringEscape e)) (toError E.StringEndless_Multi))
            )



-- CONTENT


stringContentHelp : String -> (E.Escape -> x) -> x -> List Char -> Parser x (Step (List Char) String)
stringContentHelp close toBadEscape toEndless revChars =
    oneOf
        [ Advanced.symbol close (\_ _ _ -> toEndless)
            |> andThen (\_ -> succeed (Done (String.fromList (List.reverse revChars))))
        , chompIf (\c -> c == '\n') 1
            |> andThen (\_ -> Advanced.problem toEndless)
        , succeed identity
            |> andMap (escapedChar toBadEscape)
            |> andThen (\c -> succeed (Loop (c :: revChars)))
        , chompIf (\c -> c /= '\\' && c /= '"' && c /= '\n' && c /= '\'') 1
            |> andThen (\c -> succeed (Loop (c :: revChars)))
        ]


charContentHelp : (E.Char -> x) -> List Char -> Parser x (Step (List Char) (List Char))
charContentHelp toError revChars =
    oneOf
        [ symbol "'"
            |> andThen (\_ -> succeed (Done revChars))
        , chompIf (\c -> c == '\n') 1
            |> andThen (\_ -> Advanced.problem (toError E.CharEndless))
        , succeed identity
            |> andMap (escapedChar (\e -> toError (E.CharEscape e)))
            |> andThen (\c -> succeed (Loop (c :: revChars)))
        , chompIf (\c -> c /= '\\' && c /= '\'' && c /= '\n') 1
            |> andThen (\c -> succeed (Loop (c :: revChars)))
        ]



-- ESCAPE CHARACTERS


escapedChar : (E.Escape -> x) -> Parser x Char
escapedChar toError =
    Advanced.symbol "\\" (\_ r c -> toError E.EscapeUnknown r c)
        |> andThen
            (\_ ->
                oneOf
                    [ map2 (\_ c -> c) (chompChar 'n') (succeed '\n')
                    , map2 (\_ c -> c) (chompChar 'r') (succeed '\u{000D}')
                    , map2 (\_ c -> c) (chompChar 't') (succeed '\t')
                    , map2 (\_ c -> c) (chompChar '"') (succeed '"')
                    , map2 (\_ c -> c) (chompChar '\'') (succeed '\'')
                    , map2 (\_ c -> c) (chompChar '\\') (succeed '\\')
                    , unicodeEscape toError
                    ]
            )


unicodeEscape : (E.Escape -> x) -> Parser x Char
unicodeEscape toError =
    chompChar 'u'
        |> andThen (\_ -> chompChar '{')
        |> andThen (\_ -> getChompedString (chompWhile Char.isHexDigit))
        |> andThen
            (\hex ->
                case hexToInt hex of
                    Just code ->
                        if code > 0x0010FFFF then
                            Advanced.problem (toError E.BadUnicodeCode)

                        else if String.length hex < 4 || String.length hex > 6 then
                            Advanced.problem (toError (E.BadUnicodeLength (String.length hex + 3) (String.length hex) code))

                        else
                            succeed (Char.fromCode code)

                    Nothing ->
                        Advanced.problem (toError E.BadUnicodeFormat)
            )
        |> andThen (\c -> chompChar '}' |> andThen (\_ -> succeed c))



-- HEX


hexToInt : String -> Maybe Int
hexToInt s =
    String.foldl
        (\c acc ->
            case acc of
                Nothing ->
                    Nothing

                Just n ->
                    let
                        digit =
                            if '0' <= c && c <= '9' then
                                Just (Char.toCode c - Char.toCode '0')

                            else if 'a' <= c && c <= 'f' then
                                Just (Char.toCode c - Char.toCode 'a' + 10)

                            else if 'A' <= c && c <= 'F' then
                                Just (Char.toCode c - Char.toCode 'A' + 10)

                            else
                                Nothing
                    in
                    Maybe.map (\d -> n * 16 + d) digit
        )
        (Just 0)
        s

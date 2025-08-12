module Compiler.Parse.String exposing (character, string)

import Compiler.Parse.NewPrimitives as P
import Parser.Advanced as P


character : P.Parser Char
character =
    P.succeed ()
        |. P.word1 '\'' (P.Problem_Char P.CP_Endless)
        |> P.andThen (\_ ->
            P.oneOf
                [ parseEscape
                , P.getChompedString (P.chompIf (\c -> c /= '\'') (P.Problem_Char P.CP_Endless))
                    |> P.andThen (\s ->
                        if String.length s == 1 then
                            P.succeed (String.head s |> Maybe.withDefault '?')
                        else
                            P.problem (P.Problem_Char (P.CP_NotString (String.length s)))
                    )
                ]
           )
        |> P.andThen (\c ->
            P.succeed c
                |. P.word1 '\'' (P.Problem_Char P.CP_Endless)
           )


string : P.Parser String
string =
    P.oneOf
        [ P.succeed ()
            |. P.token "\"\"\"" (P.Problem_String (P.SP_Endless_Multi))
            |> P.andThen (\_ ->
                P.getChompedString (P.chompUntil (P.Token "\"\"\"" (P.Problem_String P.SP_Endless_Multi)))
            )
        , P.succeed ()
            |. P.word1 '"' (P.Problem_String P.SP_Endless_Single)
            |> P.andThen (\_ ->
                P.getChompedString (P.chompUntil (P.Token "\"" (P.Problem_String P.SP_Endless_Single)))
            )
        ]


parseEscape : P.Parser Char
parseEscape =
    P.succeed ()
        |. P.word1 '\\' (P.Problem_Char (P.CP_Escape P.EP_Unknown))
        |> P.andThen (\_ ->
            P.oneOf
                [ P.succeed 'n' |. P.word1 'n' (P.Problem_Char (P.CP_Escape P.EP_Unknown))
                , P.succeed 'r' |. P.word1 'r' (P.Problem_Char (P.CP_Escape P.EP_Unknown))
                , P.succeed 't' |. P.word1 't' (P.Problem_Char (P.CP_Escape P.EP_Unknown))
                , P.succeed '"' |. P.word1 '"' (P.Problem_Char (P.CP_Escape P.EP_Unknown))
                , P.succeed '\'' |. P.word1 '\'' (P.Problem_Char (P.CP_Escape P.EP_Unknown))
                , P.succeed '\\' |. P.word1 '\\' (P.Problem_Char (P.CP_Escape P.EP_Unknown))
                , P.succeed ()
                    |. P.word1 'u' (P.Problem_Char (P.CP_Escape (P.EP_BadUnicodeFormat 2)))
                    |> P.andThen (\_ -> parseUnicode)
                ]
           )


parseUnicode : P.Parser Char
parseUnicode =
    P.succeed ()
        |. P.word1 '{' (P.Problem_Char (P.CP_Escape (P.EP_BadUnicodeFormat 2)))
        |> P.andThen (\_ ->
            P.getChompedString (P.chompWhile Char.isHexDigit)
                |> P.andThen (\hex ->
                    P.succeed ()
                        |. P.word1 '}' (P.Problem_Char (P.CP_Escape (P.EP_BadUnicodeFormat (String.length hex + 2))))
                        |> P.andThen (\_ ->
                            case hex of
                                "" ->
                                    P.problem (P.Problem_Char (P.CP_Escape (P.EP_BadUnicodeFormat 2)))

                                _ ->
                                    let
                                        code =
                                            fromHex hex
                                    in
                                    if code < 0 || code > 0x10FFFF then
                                        P.problem (P.Problem_Char (P.CP_Escape (P.EP_BadUnicodeCode (String.length hex + 3))))
                                    else if String.length hex < 4 || String.length hex > 6 then
                                        P.problem (P.Problem_Char (P.CP_Escape (P.EP_BadUnicodeLength (String.length hex + 3) (String.length hex) code)))
                                    else
                                        P.succeed (Char.fromCode code)
                           )
                   )
           )


fromHex : String -> Int
fromHex str =
    String.foldl (\c acc -> acc * 16 + hexDigitToInt c) 0 str


hexDigitToInt : Char -> Int
hexDigitToInt char =
    let
        code =
            Char.toCode char
    in
    if Char.isDigit char then
        code - Char.toCode '0'
    else
        Char.toCode (Char.toLower char) - Char.toCode 'a' + 10

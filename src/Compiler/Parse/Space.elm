module Compiler.Parse.Space exposing
    ( Parser
    , checkAligned
    , checkFreshLine
    , checkIndent
    , chomp
    , chompAndCheckIndent
    , docComment
    )

import Compiler.AST.Source as Src
import Compiler.Parse.NewPrimitives as P
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Parser exposing (..)
import Parser.Advanced as Advanced



-- SPACE PARSING


type alias Parser x a =
    P.Parser x ( a, A.Position )



-- CHOMP


chomp : (E.Space -> P.Row -> P.Col -> x) -> P.Parser x ()
chomp toError =
    loop ()
        (\_ ->
            oneOf
                [ chompWhile (\c -> c == ' ' || c == '\n' || c == '\u{000D}') |> andThen (\_ -> succeed (Loop ()))
                , chompComment toError |> andThen (\_ -> succeed (Loop ()))
                , succeed (Done ())
                ]
        )


chompComment : (E.Space -> P.Row -> P.Col -> x) -> P.Parser x ()
chompComment toError =
    oneOf
        [ Advanced.symbol "{-" E.EndlessMultiComment |> andThen (\_ -> loop 1 chompMultiCommentHelp)
        , Advanced.symbol "--" E.EndlessMultiComment |> andThen (\_ -> chompWhile (\c -> c /= '\n') |> andThen (\_ -> succeed ()))
        ]


chompMultiCommentHelp : Int -> P.Parser E.Space (Step Int ())
chompMultiCommentHelp openComments =
    if openComments == 0 then
        succeed (Done ())

    else
        oneOf
            [ Advanced.symbol "-}" E.EndlessMultiComment |> andThen (\_ -> succeed (Loop (openComments - 1)))
            , Advanced.symbol "{-" E.EndlessMultiComment |> andThen (\_ -> succeed (Loop (openComments + 1)))
            , chompIf (\_ -> True) 1 |> andThen (\_ -> succeed (Loop openComments))
            ]



-- CHECKS


checkIndent : A.Position -> (Int -> Int -> x) -> P.Parser x ()
checkIndent (A.Position endRow endCol) toError =
    P.getIndent
        |> andThen
            (\indent ->
                P.getCol
                    |> andThen
                        (\col ->
                            if col > indent && col > 1 then
                                succeed ()

                            else
                                Advanced.problem (toError endRow endCol)
                        )
            )


checkAligned : (Int -> Int -> Int -> x) -> P.Parser x ()
checkAligned toError =
    P.getIndent
        |> andThen
            (\indent ->
                P.getPosition
                    |> andThen
                        (\(A.Position row col) ->
                            if col == indent then
                                succeed ()

                            else
                                Advanced.problem (toError indent row col)
                        )
            )


checkFreshLine : (P.Row -> P.Col -> x) -> P.Parser x ()
checkFreshLine toError =
    P.getPosition
        |> andThen
            (\(A.Position row col) ->
                if col == 1 then
                    succeed ()

                else
                    Advanced.problem (toError row col)
            )



-- CHOMP AND CHECK


chompAndCheckIndent : (E.Space -> P.Row -> P.Col -> x) -> (P.Row -> P.Col -> x) -> P.Parser x ()
chompAndCheckIndent toSpaceError toIndentError =
    chomp toSpaceError
        |> andThen
            (\_ ->
                P.getIndent
                    |> andThen
                        (\indent ->
                            P.getPosition
                                |> andThen
                                    (\(A.Position row col) ->
                                        if col > indent && col > 1 then
                                            succeed ()

                                        else
                                            Advanced.problem (toIndentError row col)
                                    )
                        )
            )



-- DOCUMENTATION COMMENT


docComment : (Int -> Int -> x) -> (E.Space -> Int -> Int -> x) -> P.Parser x Src.Comment
docComment toExpectation toSpaceError =
    Advanced.symbol "{-|" E.EndlessMultiComment
        |> andThen
            (\_ ->
                P.getPosition
                    |> andThen
                        (\(A.Position row col) ->
                            getChompedString (chompUntil "-}")
                                |> andThen
                                    (\content ->
                                        let
                                            snippet =
                                                P.Snippet
                                                    { fptr = ""
                                                    , offset = 0
                                                    , length = String.length content
                                                    , offRow = row
                                                    , offCol = col
                                                    }
                                        in
                                        succeed (Src.Comment snippet)
                                    )
                        )
            )

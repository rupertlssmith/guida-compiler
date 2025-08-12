module Compiler.Parse.Space exposing
    ( chomp
    , checkIndent
    , checkAligned
    , checkFreshLine
    , chompAndCheckIndent
    , docComment
    )

import Compiler.AST.Source as Src
import Compiler.Parse.NewPrimitives as P
import Compiler.Reporting.Annotation as A


chomp : P.Parser ()
chomp =
    P.loop () (\_ ->
        P.oneOf
            [ P.succeed P.Loop |. P.spaces
            , P.succeed P.Loop |. P.lineComment (P.Token "--" (P.Problem_Space P.EndlessMultiComment)) -- Not quite right problem
            , P.succeed P.Loop |. P.multiComment (P.Token "{-" (P.Problem_Space P.EndlessMultiComment)) (P.Token "-}" (P.Problem_Space P.EndlessMultiComment)) P.Nestable
            , P.andThen (\_ -> P.problem (P.Problem_Space P.HasTab)) (P.chompIf ((==) '\t') (P.Problem_Space P.HasTab))
            , P.succeed (P.Done ())
            ]
    )


checkIndent : A.Position -> P.Problem -> P.Parser ()
checkIndent (A.Position endRow endCol) toError =
    P.getIndent
        |> P.andThen (\indent ->
            P.getCol
                |> P.andThen (\col ->
                    if col > indent && col > 1 then
                        P.succeed ()
                    else
                        P.problem toError
                )
        )


checkAligned : P.Problem -> P.Parser ()
checkAligned toError =
    P.getIndent
        |> P.andThen (\indent ->
            P.getCol
                |> P.andThen (\col ->
                    if col == indent then
                        P.succeed ()
                    else
                        P.problem toError
                )
        )


checkFreshLine : P.Problem -> P.Parser ()
checkFreshLine toError =
    P.getCol
        |> P.andThen (\col ->
            if col == 1 then
                P.succeed ()
            else
                P.problem toError
        )


chompAndCheckIndent : P.Problem -> P.Problem -> P.Parser ()
chompAndCheckIndent toSpaceError toIndentError =
    P.succeed ()
        |. chomp
        |> P.andThen (\_ ->
            P.getIndent
                |> P.andThen (\indent ->
                    P.getCol
                        |> P.andThen (\col ->
                            if col > indent && col > 1 then
                                P.succeed ()
                            else
                                P.problem toIndentError
                        )
                )
        )


docComment : P.Problem -> P.Problem -> P.Parser Src.Comment
docComment toExpectation toSpaceError =
    P.succeed ()
        |. P.token "{-|" toExpectation
        |> P.andThen (\_ ->
            P.getChompedString (P.chompUntil (P.Token "-}" toSpaceError))
                |> P.andThen (\str ->
                    P.getPosition
                        |> P.andThen (\(r, c) ->
                            -- This is not quite right, snippet needs more info
                            let
                                snippet =
                                    P.Snippet
                                        { fptr = ""
                                        , offset = 0
                                        , length = String.length str
                                        , offRow = r
                                        , offCol = c
                                        }
                            in
                            P.succeed (Src.Comment snippet)
                        )
                )
        )

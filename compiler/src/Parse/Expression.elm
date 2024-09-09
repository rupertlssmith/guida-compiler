module Parse.Expression exposing (expression)

import AST.Source as Src
import Data.Name as Name
import Parse.Keyword as Keyword
import Parse.Number as Number
import Parse.Pattern as Pattern
import Parse.Primitives as P
import Parse.Shader as Shader
import Parse.Space as Space
import Parse.String as String
import Parse.Symbol as Symbol
import Parse.Type as Type
import Parse.Variable as Var
import Reporting.Annotation as A
import Reporting.Error.Syntax as E



-- TERMS


term : P.Parser E.Expr Src.Expr
term =
    P.getPosition
        |> P.bind
            (\start ->
                P.oneOf E.Start
                    [ variable start |> P.bind (accessible start)
                    , string start
                    , number start
                    , Shader.shader start
                    , list start
                    , record start |> P.bind (accessible start)
                    , tuple start |> P.bind (accessible start)
                    , accessor start
                    , character start
                    ]
            )


string : A.Position -> P.Parser E.Expr Src.Expr
string start =
    String.string E.Start E.String_
        |> P.bind (\str -> P.addEnd start (Src.Str str))


character : A.Position -> P.Parser E.Expr Src.Expr
character start =
    String.character E.Start E.Char
        |> P.bind (\chr -> P.addEnd start (Src.Chr chr))


number : A.Position -> P.Parser E.Expr Src.Expr
number start =
    Number.number E.Start E.Number
        |> P.bind
            (\nmbr ->
                P.addEnd start <|
                    case nmbr of
                        Number.Int int ->
                            Src.Int int

                        Number.Float float ->
                            Src.Float float
            )


accessor : A.Position -> P.Parser E.Expr Src.Expr
accessor start =
    P.word1 '.' E.Dot
        |> P.bind (\_ -> Var.lower E.Access)
        |> P.bind (\field -> P.addEnd start (Src.Accessor field))


variable : A.Position -> P.Parser E.Expr Src.Expr
variable start =
    Var.foreignAlpha E.Start
        |> P.bind (\var -> P.addEnd start var)


accessible : A.Position -> Src.Expr -> P.Parser E.Expr Src.Expr
accessible start expr =
    P.oneOfWithFallback
        [ P.word1 '.' E.Dot
            |> P.bind (\_ -> P.getPosition)
            |> P.bind
                (\pos ->
                    Var.lower E.Access
                        |> P.bind
                            (\field ->
                                P.getPosition
                                    |> P.bind
                                        (\end ->
                                            accessible start <|
                                                A.at start end (Src.Access expr (A.at pos end field))
                                        )
                            )
                )
        ]
        expr



-- LISTS


list : A.Position -> P.Parser E.Expr Src.Expr
list start =
    P.inContext E.List (P.word1 '[' E.Start) <|
        (Space.chompAndCheckIndent E.ListSpace E.ListIndentOpen
            |> P.bind
                (\_ ->
                    P.oneOf E.ListOpen
                        [ P.specialize E.ListExpr expression
                            |> P.bind
                                (\( entry, end ) ->
                                    Space.checkIndent end E.ListIndentEnd
                                        |> P.bind (\_ -> chompListEnd start [ entry ])
                                )
                        , P.word1 ']' E.ListOpen
                            |> P.bind (\_ -> P.addEnd start (Src.List []))
                        ]
                )
        )


chompListEnd : A.Position -> List Src.Expr -> P.Parser E.List_ Src.Expr
chompListEnd start entries =
    P.oneOf E.ListEnd
        [ P.word1 ',' E.ListEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent E.ListSpace E.ListIndentExpr)
            |> P.bind (\_ -> P.specialize E.ListExpr expression)
            |> P.bind
                (\( entry, end ) ->
                    Space.checkIndent end E.ListIndentEnd
                        |> P.bind (\_ -> chompListEnd start (entry :: entries))
                )
        , P.word1 ']' E.ListEnd
            |> P.bind (\_ -> P.addEnd start (Src.List (List.reverse entries)))
        ]



-- TUPLES


tuple : A.Position -> P.Parser E.Expr Src.Expr
tuple ((A.Position row col) as start) =
    P.inContext E.Tuple (P.word1 '(' E.Start) <|
        (P.getPosition
            |> P.bind
                (\before ->
                    Space.chompAndCheckIndent E.TupleSpace E.TupleIndentExpr1
                        |> P.bind
                            (\_ ->
                                P.getPosition
                                    |> P.bind
                                        (\after ->
                                            if before /= after then
                                                P.specialize E.TupleExpr expression
                                                    |> P.bind
                                                        (\( entry, end ) ->
                                                            Space.checkIndent end E.TupleIndentEnd
                                                                |> P.bind (\_ -> chompTupleEnd start entry [])
                                                        )

                                            else
                                                P.oneOf E.TupleIndentExpr1
                                                    [ Symbol.operator E.TupleIndentExpr1 E.TupleOperatorReserved
                                                        |> P.bind
                                                            (\op ->
                                                                if op == "-" then
                                                                    P.oneOf E.TupleOperatorClose
                                                                        [ P.word1 ')' E.TupleOperatorClose
                                                                            |> P.bind (\_ -> P.addEnd start (Src.Op op))
                                                                        , P.specialize E.TupleExpr
                                                                            (term
                                                                                |> P.bind
                                                                                    (\((A.At (A.Region _ end) _) as negatedExpr) ->
                                                                                        Space.chomp E.Space
                                                                                            |> P.bind
                                                                                                (\_ ->
                                                                                                    let
                                                                                                        exprStart =
                                                                                                            A.Position row (col + 2)

                                                                                                        expr =
                                                                                                            A.at exprStart end (Src.Negate negatedExpr)
                                                                                                    in
                                                                                                    chompExprEnd exprStart
                                                                                                        (State
                                                                                                            { ops = []
                                                                                                            , expr = expr
                                                                                                            , args = []
                                                                                                            , end = end
                                                                                                            }
                                                                                                        )
                                                                                                )
                                                                                    )
                                                                            )
                                                                            |> P.bind
                                                                                (\( entry, end ) ->
                                                                                    Space.checkIndent end E.TupleIndentEnd
                                                                                        |> P.bind (\_ -> chompTupleEnd start entry [])
                                                                                )
                                                                        ]

                                                                else
                                                                    P.word1 ')' E.TupleOperatorClose
                                                                        |> P.bind (\_ -> P.addEnd start (Src.Op op))
                                                            )
                                                    , P.word1 ')' E.TupleIndentExpr1
                                                        |> P.bind (\_ -> P.addEnd start Src.Unit)
                                                    , P.specialize E.TupleExpr expression
                                                        |> P.bind
                                                            (\( entry, end ) ->
                                                                Space.checkIndent end E.TupleIndentEnd
                                                                    |> P.bind (\_ -> chompTupleEnd start entry [])
                                                            )
                                                    ]
                                        )
                            )
                )
        )


chompTupleEnd : A.Position -> Src.Expr -> List Src.Expr -> P.Parser E.Tuple Src.Expr
chompTupleEnd start firstExpr revExprs =
    P.oneOf E.TupleEnd
        [ P.word1 ',' E.TupleEnd
            |> P.bind
                (\_ ->
                    Space.chompAndCheckIndent E.TupleSpace E.TupleIndentExprN
                        |> P.bind
                            (\_ ->
                                P.specialize E.TupleExpr expression
                                    |> P.bind
                                        (\( entry, end ) ->
                                            Space.checkIndent end E.TupleIndentEnd
                                                |> P.bind (\_ -> chompTupleEnd start firstExpr (entry :: revExprs))
                                        )
                            )
                )
        , P.word1 ')' E.TupleEnd
            |> P.bind
                (\_ ->
                    case List.reverse revExprs of
                        [] ->
                            P.pure firstExpr

                        secondExpr :: otherExprs ->
                            P.addEnd start (Src.Tuple firstExpr secondExpr otherExprs)
                )
        ]



-- RECORDS


record : A.Position -> P.Parser E.Expr Src.Expr
record start =
    P.inContext E.Record (P.word1 '{' E.Start) <|
        (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentOpen
            |> P.bind
                (\_ ->
                    P.oneOf E.RecordOpen
                        [ P.word1 '}' E.RecordOpen
                            |> P.bind (\_ -> P.addEnd start (Src.Record []))
                        , P.addLocation (Var.lower E.RecordField)
                            |> P.bind
                                (\starter ->
                                    Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals
                                        |> P.bind
                                            (\_ ->
                                                P.oneOf E.RecordEquals
                                                    [ P.word1 '|' E.RecordEquals
                                                        |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField)
                                                        |> P.bind (\_ -> chompField)
                                                        |> P.bind (\firstField -> chompFields [ firstField ])
                                                        |> P.bind (\fields -> P.addEnd start (Src.Update starter fields))
                                                    , P.word1 '=' E.RecordEquals
                                                        |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr)
                                                        |> P.bind (\_ -> P.specialize E.RecordExpr expression)
                                                        |> P.bind
                                                            (\( value, end ) ->
                                                                Space.checkIndent end E.RecordIndentEnd
                                                                    |> P.bind (\_ -> chompFields [ ( starter, value ) ])
                                                                    |> P.bind (\fields -> P.addEnd start (Src.Record fields))
                                                            )
                                                    ]
                                            )
                                )
                        ]
                )
        )


type alias Field =
    ( A.Located Name.Name, Src.Expr )


chompFields : List Field -> P.Parser E.Record (List Field)
chompFields fields =
    P.oneOf E.RecordEnd
        [ P.word1 ',' E.RecordEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField)
            |> P.bind (\_ -> chompField)
            |> P.bind (\f -> chompFields (f :: fields))
        , P.word1 '}' E.RecordEnd
            |> P.fmap (\_ -> List.reverse fields)
        ]


chompField : P.Parser E.Record Field
chompField =
    P.addLocation (Var.lower E.RecordField)
        |> P.bind
            (\key ->
                Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals
                    |> P.bind (\_ -> P.word1 '=' E.RecordEquals)
                    |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr)
                    |> P.bind (\_ -> P.specialize E.RecordExpr expression)
                    |> P.bind
                        (\( value, end ) ->
                            Space.checkIndent end E.RecordIndentEnd
                                |> P.fmap (\_ -> ( key, value ))
                        )
            )



-- EXPRESSIONS


expression : Space.Parser E.Expr Src.Expr
expression =
    P.getPosition
        |> P.bind
            (\start ->
                P.oneOf E.Start
                    [ let_ start
                    , if_ start
                    , case_ start
                    , function start
                    , possiblyNegativeTerm start
                        |> P.bind
                            (\expr ->
                                P.getPosition
                                    |> P.bind
                                        (\end ->
                                            Space.chomp E.Space
                                                |> P.bind
                                                    (\_ ->
                                                        chompExprEnd start
                                                            (State
                                                                { ops = []
                                                                , expr = expr
                                                                , args = []
                                                                , end = end
                                                                }
                                                            )
                                                    )
                                        )
                            )
                    ]
            )


type State
    = State
        { ops : List ( Src.Expr, A.Located Name.Name )
        , expr : Src.Expr
        , args : List Src.Expr
        , end : A.Position
        }


chompExprEnd : A.Position -> State -> Space.Parser E.Expr Src.Expr
chompExprEnd start (State { ops, expr, args, end }) =
    P.oneOfWithFallback
        [ -- argument
          Space.checkIndent end E.Start
            |> P.bind (\_ -> term)
            |> P.bind
                (\arg ->
                    P.getPosition
                        |> P.bind
                            (\newEnd ->
                                Space.chomp E.Space
                                    |> P.bind
                                        (\_ ->
                                            chompExprEnd start
                                                (State
                                                    { ops = ops
                                                    , expr = expr
                                                    , args = arg :: args
                                                    , end = newEnd
                                                    }
                                                )
                                        )
                            )
                )
        , -- operator
          Space.checkIndent end E.Start
            |> P.bind (\_ -> P.addLocation (Symbol.operator E.Start E.OperatorReserved))
            |> P.bind
                (\((A.At (A.Region opStart opEnd) opName) as op) ->
                    Space.chompAndCheckIndent E.Space (E.IndentOperatorRight opName)
                        |> P.bind (\_ -> P.getPosition)
                        |> P.bind
                            (\newStart ->
                                if "-" == opName && end /= opStart && opEnd == newStart then
                                    -- negative terms
                                    term
                                        |> P.bind
                                            (\negatedExpr ->
                                                P.getPosition
                                                    |> P.bind
                                                        (\newEnd ->
                                                            Space.chomp E.Space
                                                                |> P.bind
                                                                    (\_ ->
                                                                        let
                                                                            arg =
                                                                                A.at opStart newEnd (Src.Negate negatedExpr)
                                                                        in
                                                                        chompExprEnd start
                                                                            (State
                                                                                { ops = ops
                                                                                , expr = expr
                                                                                , args = arg :: args
                                                                                , end = newEnd
                                                                                }
                                                                            )
                                                                    )
                                                        )
                                            )

                                else
                                    let
                                        err =
                                            E.OperatorRight opName
                                    in
                                    P.oneOf err
                                        [ -- term
                                          possiblyNegativeTerm newStart
                                            |> P.bind
                                                (\newExpr ->
                                                    P.getPosition
                                                        |> P.bind
                                                            (\newEnd ->
                                                                Space.chomp E.Space
                                                                    |> P.bind
                                                                        (\_ ->
                                                                            let
                                                                                newOps =
                                                                                    ( toCall expr args, op ) :: ops
                                                                            in
                                                                            chompExprEnd start
                                                                                (State
                                                                                    { ops = newOps
                                                                                    , expr = newExpr
                                                                                    , args = []
                                                                                    , end = newEnd
                                                                                    }
                                                                                )
                                                                        )
                                                            )
                                                )
                                        , -- final term
                                          P.oneOf err
                                            [ let_ newStart
                                            , case_ newStart
                                            , if_ newStart
                                            , function newStart
                                            ]
                                            |> P.fmap
                                                (\( newLast, newEnd ) ->
                                                    let
                                                        newOps =
                                                            ( toCall expr args, op ) :: ops

                                                        finalExpr =
                                                            Src.Binops (List.reverse newOps) newLast
                                                    in
                                                    ( A.at start newEnd finalExpr, newEnd )
                                                )
                                        ]
                            )
                )
        ]
        -- done
        (case ops of
            [] ->
                ( toCall expr args
                , end
                )

            _ ->
                ( A.at start end (Src.Binops (List.reverse ops) (toCall expr args))
                , end
                )
        )


possiblyNegativeTerm : A.Position -> P.Parser E.Expr Src.Expr
possiblyNegativeTerm start =
    P.oneOf E.Start
        [ P.word1 '-' E.Start
            |> P.bind
                (\_ ->
                    term
                        |> P.bind
                            (\expr ->
                                P.addEnd start (Src.Negate expr)
                            )
                )
        , term
        ]


toCall : Src.Expr -> List Src.Expr -> Src.Expr
toCall func revArgs =
    case revArgs of
        [] ->
            func

        lastArg :: _ ->
            A.merge func lastArg (Src.Call func (List.reverse revArgs))



-- IF EXPRESSION


if_ : A.Position -> Space.Parser E.Expr Src.Expr
if_ start =
    P.inContext E.If (Keyword.if_ E.Start) <|
        chompIfEnd start []


chompIfEnd : A.Position -> List ( Src.Expr, Src.Expr ) -> Space.Parser E.If Src.Expr
chompIfEnd start branches =
    Space.chompAndCheckIndent E.IfSpace E.IfIndentCondition
        |> P.bind (\_ -> P.specialize E.IfCondition expression)
        |> P.bind
            (\( condition, condEnd ) ->
                Space.checkIndent condEnd E.IfIndentThen
                    |> P.bind (\_ -> Keyword.then_ E.IfThen)
                    |> P.bind (\_ -> Space.chompAndCheckIndent E.IfSpace E.IfIndentThenBranch)
                    |> P.bind (\_ -> P.specialize E.IfThenBranch expression)
                    |> P.bind
                        (\( thenBranch, thenEnd ) ->
                            Space.checkIndent thenEnd E.IfIndentElse
                                |> P.bind (\_ -> Keyword.else_ E.IfElse)
                                |> P.bind (\_ -> Space.chompAndCheckIndent E.IfSpace E.IfIndentElseBranch)
                                |> P.bind
                                    (\_ ->
                                        let
                                            newBranches =
                                                ( condition, thenBranch ) :: branches
                                        in
                                        P.oneOf E.IfElseBranchStart
                                            [ Keyword.if_ E.IfElseBranchStart
                                                |> P.bind (\_ -> chompIfEnd start newBranches)
                                            , P.specialize E.IfElseBranch expression
                                                |> P.fmap
                                                    (\( elseBranch, elseEnd ) ->
                                                        let
                                                            ifExpr =
                                                                Src.If (List.reverse newBranches) elseBranch
                                                        in
                                                        ( A.at start elseEnd ifExpr, elseEnd )
                                                    )
                                            ]
                                    )
                        )
            )



-- LAMBDA EXPRESSION


function : A.Position -> Space.Parser E.Expr Src.Expr
function start =
    P.inContext E.Func (P.word1 '\\' E.Start) <|
        (Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArg
            |> P.bind (\_ -> P.specialize E.FuncArg Pattern.term)
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArrow
                        |> P.bind (\_ -> chompArgs [ arg ])
                        |> P.bind
                            (\revArgs ->
                                Space.chompAndCheckIndent E.FuncSpace E.FuncIndentBody
                                    |> P.bind (\_ -> P.specialize E.FuncBody expression)
                                    |> P.fmap
                                        (\( body, end ) ->
                                            let
                                                funcExpr =
                                                    Src.Lambda (List.reverse revArgs) body
                                            in
                                            ( A.at start end funcExpr, end )
                                        )
                            )
                )
        )


chompArgs : List Src.Pattern -> P.Parser E.Func (List Src.Pattern)
chompArgs revArgs =
    P.oneOf E.FuncArrow
        [ P.specialize E.FuncArg Pattern.term
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArrow
                        |> P.bind (\_ -> chompArgs (arg :: revArgs))
                )
        , P.word2 '-' '>' E.FuncArrow
            |> P.fmap (\_ -> revArgs)
        ]



-- CASE EXPRESSIONS


case_ : A.Position -> Space.Parser E.Expr Src.Expr
case_ start =
    P.inContext E.Case (Keyword.case_ E.Start) <|
        (Space.chompAndCheckIndent E.CaseSpace E.CaseIndentExpr
            |> P.bind (\_ -> P.specialize E.CaseExpr expression)
            |> P.bind
                (\( expr, exprEnd ) ->
                    Space.checkIndent exprEnd E.CaseIndentOf
                        |> P.bind (\_ -> Keyword.of_ E.CaseOf)
                        |> P.bind (\_ -> Space.chompAndCheckIndent E.CaseSpace E.CaseIndentPattern)
                        |> P.bind
                            (\_ ->
                                P.withIndent <|
                                    (chompBranch
                                        |> P.bind
                                            (\( firstBranch, firstEnd ) ->
                                                chompCaseEnd [ firstBranch ] firstEnd
                                                    |> P.fmap
                                                        (\( branches, end ) ->
                                                            ( A.at start end (Src.Case expr branches)
                                                            , end
                                                            )
                                                        )
                                            )
                                    )
                            )
                )
        )


chompBranch : Space.Parser E.Case ( Src.Pattern, Src.Expr )
chompBranch =
    P.specialize E.CasePattern Pattern.expression
        |> P.bind
            (\( pattern, patternEnd ) ->
                Space.checkIndent patternEnd E.CaseIndentArrow
                    |> P.bind (\_ -> P.word2 '-' '>' E.CaseArrow)
                    |> P.bind (\_ -> Space.chompAndCheckIndent E.CaseSpace E.CaseIndentBranch)
                    |> P.bind (\_ -> P.specialize E.CaseBranch expression)
                    |> P.fmap (\( branchExpr, end ) -> ( ( pattern, branchExpr ), end ))
            )


chompCaseEnd : List ( Src.Pattern, Src.Expr ) -> A.Position -> Space.Parser E.Case (List ( Src.Pattern, Src.Expr ))
chompCaseEnd branches end =
    P.oneOfWithFallback
        [ Space.checkAligned E.CasePatternAlignment
            |> P.bind (\_ -> chompBranch)
            |> P.bind (\( branch, newEnd ) -> chompCaseEnd (branch :: branches) newEnd)
        ]
        ( List.reverse branches, end )



-- LET EXPRESSION


let_ : A.Position -> Space.Parser E.Expr Src.Expr
let_ start =
    P.inContext E.Let (Keyword.let_ E.Start) <|
        ((P.withBacksetIndent 3 <|
            (Space.chompAndCheckIndent E.LetSpace E.LetIndentDef
                |> P.bind
                    (\_ ->
                        P.withIndent <|
                            (chompLetDef
                                |> P.bind (\( def, end ) -> chompLetDefs [ def ] end)
                            )
                    )
            )
         )
            |> P.bind
                (\( defs, defsEnd ) ->
                    Space.checkIndent defsEnd E.LetIndentIn
                        |> P.bind (\_ -> Keyword.in_ E.LetIn)
                        |> P.bind (\_ -> Space.chompAndCheckIndent E.LetSpace E.LetIndentBody)
                        |> P.bind (\_ -> P.specialize E.LetBody expression)
                        |> P.fmap
                            (\( body, end ) ->
                                ( A.at start end (Src.Let defs body), end )
                            )
                )
        )


chompLetDefs : List (A.Located Src.Def) -> A.Position -> Space.Parser E.Let (List (A.Located Src.Def))
chompLetDefs revDefs end =
    P.oneOfWithFallback
        [ Space.checkAligned E.LetDefAlignment
            |> P.bind (\_ -> chompLetDef)
            |> P.bind (\( def, newEnd ) -> chompLetDefs (def :: revDefs) newEnd)
        ]
        ( List.reverse revDefs, end )



-- LET DEFINITIONS


chompLetDef : Space.Parser E.Let (A.Located Src.Def)
chompLetDef =
    P.oneOf E.LetDefName
        [ definition
        , destructure
        ]



-- DEFINITION


definition : Space.Parser E.Let (A.Located Src.Def)
definition =
    P.addLocation (Var.lower E.LetDefName)
        |> P.bind
            (\((A.At (A.Region start _) name) as aname) ->
                P.specialize (E.LetDef name) <|
                    (Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
                        |> P.bind
                            (\_ ->
                                P.oneOf E.DefEquals
                                    [ P.word1 ':' E.DefEquals
                                        |> P.bind (\_ -> Space.chompAndCheckIndent E.DefSpace E.DefIndentType)
                                        |> P.bind (\_ -> P.specialize E.DefType Type.expression)
                                        |> P.bind
                                            (\( tipe, _ ) ->
                                                Space.checkAligned E.DefAlignment
                                                    |> P.bind (\_ -> chompMatchingName name)
                                                    |> P.bind
                                                        (\defName ->
                                                            Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
                                                                |> P.bind (\_ -> chompDefArgsAndBody start defName (Just tipe) [])
                                                        )
                                            )
                                    , chompDefArgsAndBody start aname Nothing []
                                    ]
                            )
                    )
            )


chompDefArgsAndBody : A.Position -> A.Located Name.Name -> Maybe Src.Type -> List Src.Pattern -> Space.Parser E.Def (A.Located Src.Def)
chompDefArgsAndBody start name tipe revArgs =
    P.oneOf E.DefEquals
        [ P.specialize E.DefArg Pattern.term
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
                        |> P.bind (\_ -> chompDefArgsAndBody start name tipe (arg :: revArgs))
                )
        , P.word1 '=' E.DefEquals
            |> P.bind (\_ -> Space.chompAndCheckIndent E.DefSpace E.DefIndentBody)
            |> P.bind (\_ -> P.specialize E.DefBody expression)
            |> P.fmap
                (\( body, end ) ->
                    ( A.at start end (Src.Define name (List.reverse revArgs) body tipe)
                    , end
                    )
                )
        ]


chompMatchingName : Name.Name -> P.Parser E.Def (A.Located Name.Name)
chompMatchingName expectedName =
    let
        (P.Parser parserL) =
            Var.lower E.DefNameRepeat
    in
    P.Parser <|
        \((P.State _ _ _ _ sr sc) as state) ->
            Result.andThen
                (\(P.POk status name ((P.State _ _ _ _ er ec) as newState)) ->
                    if expectedName == name then
                        Ok (P.POk status (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState)

                    else
                        Err (P.PErr status sr sc (E.DefNameMatch name))
                )
                (parserL state)



-- DESTRUCTURE


destructure : Space.Parser E.Let (A.Located Src.Def)
destructure =
    P.specialize E.LetDestruct <|
        (P.getPosition
            |> P.bind
                (\start ->
                    P.specialize E.DestructPattern Pattern.term
                        |> P.bind
                            (\pattern ->
                                Space.chompAndCheckIndent E.DestructSpace E.DestructIndentEquals
                                    |> P.bind (\_ -> P.word1 '=' E.DestructEquals)
                                    |> P.bind (\_ -> Space.chompAndCheckIndent E.DestructSpace E.DestructIndentBody)
                                    |> P.bind (\_ -> P.specialize E.DestructBody expression)
                                    |> P.fmap
                                        (\( expr, end ) ->
                                            ( A.at start end (Src.Destruct pattern expr)
                                            , end
                                            )
                                        )
                            )
                )
        )

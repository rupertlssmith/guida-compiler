module Compiler.Parse.Expression exposing
    ( expression
    , record
    )

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.NewPrimitives as P
import Compiler.Parse.Number as Number
import Compiler.Parse.Pattern as Pattern
import Compiler.Parse.Shader as Shader
import Compiler.Parse.Space as Space
import Compiler.Parse.String as String
import Compiler.Parse.Symbol as Symbol
import Compiler.Parse.SyntaxVersion as SV exposing (SyntaxVersion)
import Compiler.Parse.Type as Type
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A


term : SyntaxVersion -> P.Parser Src.Expr
term syntaxVersion =
    P.oneOf
        [ P.addLocation (variable |> P.andThen accessible)
        , P.addLocation string
        , P.addLocation number
        , Shader.shader
        , list syntaxVersion
        , record syntaxVersion |> P.andThen accessible
        , tuple syntaxVersion |> P.andThen accessible
        , P.addLocation accessor
        , P.addLocation character
        ]


string : P.Parser Src.Expr_
string =
    String.string
        |> P.map Src.Str


character : P.Parser Src.Expr_
character =
    String.character
        |> P.map Src.Chr


number : P.Parser Src.Expr_
number =
    Number.number
        |> P.map
            (\nmbr ->
                case nmbr of
                    Number.Int int ->
                        Src.Int int

                    Number.Float float ->
                        Src.Float float
            )


accessor : P.Parser Src.Expr_
accessor =
    P.succeed ()
        |. P.word1 '.' (P.Problem_Expr P.EP_Dot)
        |> P.andThen (\_ -> Var.lower (P.Problem_Expr P.EP_Access))
        |> P.map Src.Accessor


variable : P.Parser Src.Expr
variable =
    Var.foreignAlpha (P.Problem_Expr P.EP_Start)
        |> P.andThen (\var -> P.addLocation (P.succeed var))


accessible : Src.Expr -> P.Parser Src.Expr
accessible expr =
    P.oneOfWithFallback
        [ P.succeed ()
            |. P.word1 '.' (P.Problem_Expr P.EP_Dot)
            |> P.andThen (\_ ->
                P.addLocation (Var.lower (P.Problem_Expr P.EP_Access))
                    |> P.andThen (\field ->
                        accessible (A.merge expr field (Src.Access expr field))
                       )
               )
        ]
        expr



-- LISTS


list : SyntaxVersion -> P.Parser Src.Expr
list syntaxVersion =
    P.inContext (P.CtxNode P.NList)
        (P.succeed ()
            |. P.word1 '[' (P.Problem_Expr (P.EP_List P.LP_Open))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        P.oneOf
                            [ expression syntaxVersion
                                |> P.andThen (\entry ->
                                    Space.chomp
                                        |> P.andThen (\_ -> P.loop [ entry ] (chompListEnd syntaxVersion))
                                   )
                            , P.addLocation (P.succeed (Src.List []))
                                |. P.word1 ']' (P.Problem_Expr (P.EP_List P.LP_End))
                            ]
                       )
               )
        )


chompListEnd : SyntaxVersion -> List Src.Expr -> P.Parser (P.Step (List Src.Expr) Src.Expr)
chompListEnd syntaxVersion entries =
    P.oneOf
        [ P.succeed ()
            |. P.word1 ',' (P.Problem_Expr (P.EP_List P.LP_End))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        expression syntaxVersion
                            |> P.andThen (\entry ->
                                Space.chomp
                                    |> P.andThen (\_ -> P.succeed (P.Loop (entry :: entries)))
                               )
                       )
               )
        , P.addLocation (P.succeed (Src.List (List.reverse entries)))
            |. P.word1 ']' (P.Problem_Expr (P.EP_List P.LP_End))
            |> P.map P.Done
        ]



-- TUPLES


tuple : SyntaxVersion -> P.Parser Src.Expr
tuple syntaxVersion =
    P.inContext (P.CtxNode P.NParens)
        (P.succeed ()
            |. P.word1 '(' (P.Problem_Expr (P.EP_Tuple P.TUP_IndentExpr1))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        P.oneOf
                            [ Symbol.operator (P.Problem_Expr (P.EP_Tuple P.TUP_IndentExpr1)) (\op -> P.Problem_Expr (P.EP_Tuple (P.TUP_OperatorReserved op)))
                                |> P.andThen (\op ->
                                    if op == "-" then
                                        P.oneOf
                                            [ P.addLocation (P.succeed (Src.Op op))
                                                |. P.word1 ')' (P.Problem_Expr (P.EP_Tuple P.TUP_OperatorClose))
                                            , term syntaxVersion
                                                |> P.andThen (\negatedExpr ->
                                                    Space.chomp
                                                        |> P.andThen (\_ ->
                                                            let
                                                                expr =
                                                                    A.map Src.Negate negatedExpr
                                                            in
                                                            chompExprEnd syntaxVersion expr
                                                           )
                                                   )
                                                |> P.andThen (\entry -> chompTupleEnd syntaxVersion entry [])
                                            ]
                                    else
                                        P.addLocation (P.succeed (Src.Op op))
                                            |. P.word1 ')' (P.Problem_Expr (P.EP_Tuple P.TUP_OperatorClose))
                                   )
                            , P.addLocation (P.succeed Src.Unit)
                                |. P.word1 ')' (P.Problem_Expr (P.EP_Tuple P.TUP_IndentExpr1))
                            , expression syntaxVersion
                                |> P.andThen (\entry ->
                                    Space.chomp
                                        |> P.andThen (\_ -> chompTupleEnd syntaxVersion entry [])
                                   )
                            ]
                       )
               )
        )


chompTupleEnd : SyntaxVersion -> Src.Expr -> List Src.Expr -> P.Parser Src.Expr
chompTupleEnd syntaxVersion firstExpr revExprs =
    P.oneOf
        [ P.succeed ()
            |. P.word1 ',' (P.Problem_Expr (P.EP_Tuple P.TUP_End))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        expression syntaxVersion
                            |> P.andThen (\entry -> chompTupleEnd syntaxVersion firstExpr (entry :: revExprs))
                       )
               )
        , P.succeed ()
            |. P.word1 ')' (P.Problem_Expr (P.EP_Tuple P.TUP_End))
            |> P.map (\_ ->
                case List.reverse revExprs of
                    [] ->
                        firstExpr

                    secondExpr :: otherExprs ->
                        A.merge firstExpr (List.head otherExprs |> Maybe.withDefault secondExpr) (Src.Tuple firstExpr secondExpr otherExprs)
               )
        ]



-- RECORDS


record : SyntaxVersion -> P.Parser Src.Expr
record syntaxVersion =
    P.inContext (P.CtxNode P.NRecord) <|
        (P.succeed ()
            |. P.word1 '{' (P.Problem_Expr (P.EP_Record P.RP_Open))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        P.oneOf
                            [ P.addLocation (P.succeed (Src.Record []))
                                |. P.word1 '}' (P.Problem_Expr (P.EP_Record P.RP_End))
                            , P.addLocation (Var.lower (P.Problem_Expr (P.EP_Record P.RP_Field)))
                                |> P.andThen (\starter ->
                                    Space.chomp
                                        |> P.andThen (\_ ->
                                            P.oneOf
                                                [ P.succeed ()
                                                    |. P.word1 '|' (P.Problem_Expr (P.EP_Record P.RP_Equals))
                                                    |> P.andThen (\_ ->
                                                        Space.chomp
                                                            |> P.andThen (\_ -> chompField syntaxVersion)
                                                            |> P.andThen (\firstField -> chompFields syntaxVersion [ firstField ])
                                                            |> P.map (\fields -> A.map (\(A.At region (Src.Var _ name)) -> Src.Update (A.At region (Src.Var Src.LowVar name)) fields) starter)
                                                       )
                                                , P.succeed ()
                                                    |. P.word1 '=' (P.Problem_Expr (P.EP_Record P.RP_Equals))
                                                    |> P.andThen (\_ ->
                                                        Space.chomp
                                                            |> P.andThen (\_ -> expression syntaxVersion)
                                                            |> P.andThen (\value ->
                                                                Space.chomp
                                                                    |> P.andThen (\_ -> chompFields syntaxVersion [ ( starter, value ) ])
                                                                    |> P.map (\fields -> A.map (\_ -> Src.Record fields) starter)
                                                               )
                                                       )
                                                ]
                                           )
                                   )
                            ]
                       )
               )
        )


type alias Field =
    ( A.Located Name.Name, Src.Expr )


chompFields : SyntaxVersion -> List Field -> P.Parser (List Field)
chompFields syntaxVersion fields =
    P.oneOfWithFallback
        [ P.succeed ()
            |. P.word1 ',' (P.Problem_Expr (P.EP_Record P.RP_End))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        chompField syntaxVersion
                            |> P.andThen (\f -> chompFields syntaxVersion (f :: fields))
                       )
               )
        , P.succeed (List.reverse fields)
            |. P.word1 '}' (P.Problem_Expr (P.EP_Record P.RP_End))
        ]
        (List.reverse fields)


chompField : SyntaxVersion -> P.Parser Field
chompField syntaxVersion =
    P.addLocation (Var.lower (P.Problem_Expr (P.EP_Record P.RP_Field)))
        |> P.andThen (\key ->
            Space.chomp
                |> P.andThen (\_ ->
                    P.succeed ()
                        |. P.word1 '=' (P.Problem_Expr (P.EP_Record P.RP_Equals))
                        |> P.andThen (\_ ->
                            Space.chomp
                                |> P.andThen (\_ ->
                                    expression syntaxVersion
                                        |> P.map (\value -> ( key, value ))
                                   )
                           )
                   )
           )



-- EXPRESSIONS


expression : SyntaxVersion -> P.Parser Src.Expr
expression syntaxVersion =
    P.oneOf
        [ let_ syntaxVersion
        , if_ syntaxVersion
        , case_ syntaxVersion
        , function_ syntaxVersion
        , possiblyNegativeTerm syntaxVersion
            |> P.andThen (\expr ->
                Space.chomp
                    |> P.andThen (\_ -> chompExprEnd syntaxVersion expr)
               )
        ]


chompExprEnd : SyntaxVersion -> Src.Expr -> P.Parser Src.Expr
chompExprEnd syntaxVersion expr =
    P.loop { ops = [], expr = expr, args = [] } (chompExprEndHelp syntaxVersion)


type alias ChompExprState =
    { ops : List ( Src.Expr, A.Located Name.Name )
    , expr : Src.Expr
    , args : List Src.Expr
    }


chompExprEndHelp : SyntaxVersion -> ChompExprState -> P.Parser (P.Step ChompExprState Src.Expr)
chompExprEndHelp syntaxVersion state =
    P.oneOfWithFallback
        [ -- argument
          term syntaxVersion
            |> P.andThen (\arg ->
                Space.chomp
                    |> P.andThen (\_ ->
                        P.succeed (P.Loop { state | args = arg :: state.args })
                       )
               )
        , -- operator
          P.addLocation (Symbol.operator (P.Problem_Expr P.EP_Start) (\op -> P.Problem_Expr (P.EP_OperatorReserved op)))
            |> P.andThen (\op ->
                Space.chomp
                    |> P.andThen (\_ ->
                        let
                            newOps =
                                ( toCall state.expr state.args, op ) :: state.ops
                        in
                        possiblyNegativeTerm syntaxVersion
                            |> P.andThen (\newExpr ->
                                P.succeed (P.Loop { ops = newOps, expr = newExpr, args = [] })
                               )
                       )
               )
        ]
        (P.Done <|
            let
                finalExpr =
                    toCall state.expr state.args
            in
            if List.isEmpty state.ops then
                finalExpr
            else
                A.merge (List.head state.ops |> Maybe.map Tuple.second |> Maybe.withDefault (A.sameAs finalExpr "")) finalExpr (Src.Binops (List.reverse state.ops) finalExpr)
        )


possiblyNegativeTerm : SyntaxVersion -> P.Parser Src.Expr
possiblyNegativeTerm syntaxVersion =
    P.oneOf
        [ P.succeed ()
            |. P.word1 '-' (P.Problem_Expr P.EP_Start)
            |> P.andThen (\_ ->
                term syntaxVersion
                    |> P.map (A.map Src.Negate)
               )
        , term syntaxVersion
        ]


toCall : Src.Expr -> List Src.Expr -> Src.Expr
toCall func revArgs =
    case revArgs of
        [] ->
            func

        firstArg :: _ ->
            A.merge func firstArg (Src.Call func (List.reverse revArgs))



-- IF EXPRESSION


if_ : SyntaxVersion -> P.Parser Src.Expr
if_ syntaxVersion =
    P.inContext (P.CtxNode P.NCond)
        (P.succeed ()
            |. Keyword.if_ (P.Problem_Expr (P.EP_If P.IP_Space))
            |> P.andThen (\_ -> chompIfEnd syntaxVersion [])
        )


chompIfEnd : SyntaxVersion -> List ( Src.Expr, Src.Expr ) -> P.Parser Src.Expr
chompIfEnd syntaxVersion branches =
    Space.chomp
        |> P.andThen (\_ ->
            expression syntaxVersion
                |> P.andThen (\condition ->
                    Space.chomp
                        |> P.andThen (\_ ->
                            P.succeed ()
                                |. Keyword.then_ (P.Problem_Expr (P.EP_If P.IP_Then))
                                |> P.andThen (\_ ->
                                    Space.chomp
                                        |> P.andThen (\_ ->
                                            expression syntaxVersion
                                                |> P.andThen (\thenBranch ->
                                                    Space.chomp
                                                        |> P.andThen (\_ ->
                                                            P.succeed ()
                                                                |. Keyword.else_ (P.Problem_Expr (P.EP_If P.IP_Else))
                                                                |> P.andThen (\_ ->
                                                                    Space.chomp
                                                                        |> P.andThen (\_ ->
                                                                            let
                                                                                newBranches =
                                                                                    ( condition, thenBranch ) :: branches
                                                                            in
                                                                            P.oneOf
                                                                                [ P.succeed ()
                                                                                    |. Keyword.if_ (P.Problem_Expr (P.EP_If P.IP_ElseBranchStart))
                                                                                    |> P.andThen (\_ -> chompIfEnd syntaxVersion newBranches)
                                                                                , expression syntaxVersion
                                                                                    |> P.map (\elseBranch ->
                                                                                        A.merge condition elseBranch (Src.If (List.reverse newBranches) elseBranch)
                                                                                       )
                                                                                ]
                                                                           )
                                                                   )
                                                           )
                                                   )
                                           )
                                   )
                           )
                   )
           )



-- LAMBDA EXPRESSION


function_ : SyntaxVersion -> P.Parser Src.Expr
function_ syntaxVersion =
    P.inContext (P.CtxNode P.NFunc)
        (P.succeed ()
            |. P.word1 '\\' (P.Problem_Expr (P.EP_Func P.FP_Space))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        Pattern.term syntaxVersion
                            |> P.andThen (\arg ->
                                Space.chomp
                                    |> P.andThen (\_ -> chompArgs syntaxVersion [ arg ])
                                    |> P.andThen (\revArgs ->
                                        Space.chomp
                                            |> P.andThen (\_ ->
                                                expression syntaxVersion
                                                    |> P.map (\body ->
                                                        A.merge (List.head revArgs |> Maybe.withDefault (A.sameAs body "")) body (Src.Lambda (List.reverse revArgs) body)
                                                       )
                                               )
                                       )
                               )
                       )
               )
        )


chompArgs : SyntaxVersion -> List Src.Pattern -> P.Parser (List Src.Pattern)
chompArgs syntaxVersion revArgs =
    P.oneOf
        [ Pattern.term syntaxVersion
            |> P.andThen (\arg ->
                Space.chomp
                    |> P.andThen (\_ -> chompArgs syntaxVersion (arg :: revArgs))
               )
        , P.succeed (List.reverse revArgs)
            |. P.word2 '-' '>' (P.Problem_Expr (P.EP_Func P.FP_Arrow))
        ]



-- CASE EXPRESSIONS


case_ : SyntaxVersion -> P.Parser Src.Expr
case_ syntaxVersion =
    P.inContext (P.CtxNode P.NCase)
        (P.succeed ()
            |. Keyword.case_ (P.Problem_Expr (P.EP_Case P.CP_Space))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        expression syntaxVersion
                            |> P.andThen (\expr ->
                                Space.chomp
                                    |> P.andThen (\_ ->
                                        P.succeed ()
                                            |. Keyword.of_ (P.Problem_Expr (P.EP_Case P.CP_Of))
                                            |> P.andThen (\_ ->
                                                Space.chomp
                                                    |> P.andThen (\_ ->
                                                        P.withIndent
                                                            (chompBranch syntaxVersion
                                                                |> P.andThen (\firstBranch ->
                                                                    chompCaseEnd syntaxVersion [ firstBranch ]
                                                                        |> P.map (\branches ->
                                                                            A.merge expr (List.head branches |> Maybe.map Tuple.second |> Maybe.withDefault (A.sameAs expr "")) (Src.Case expr branches)
                                                                           )
                                                                   )
                                                            )
                                                       )
                                               )
                                       )
                               )
                       )
               )
        )


chompBranch : SyntaxVersion -> P.Parser ( Src.Pattern, Src.Expr )
chompBranch syntaxVersion =
    Pattern.expression syntaxVersion
        |> P.andThen (\pattern ->
            Space.chomp
                |> P.andThen (\_ ->
                    P.succeed ()
                        |. P.word2 '-' '>' (P.Problem_Expr (P.EP_Case P.CP_Arrow))
                        |> P.andThen (\_ ->
                            Space.chomp
                                |> P.andThen (\_ ->
                                    expression syntaxVersion
                                        |> P.map (\branchExpr -> ( pattern, branchExpr ))
                                   )
                           )
                   )
           )


chompCaseEnd : SyntaxVersion -> List ( Src.Pattern, Src.Expr ) -> P.Parser (List ( Src.Pattern, Src.Expr ))
chompCaseEnd syntaxVersion branches =
    P.oneOfWithFallback
        [ Space.checkAligned (P.Problem_Expr (P.EP_Case (P.CP_PatternAlignment 0))) -- dummy alignment
            |> P.andThen (\_ ->
                chompBranch syntaxVersion
                    |> P.andThen (\branch -> chompCaseEnd syntaxVersion (branch :: branches))
               )
        ]
        (List.reverse branches)



-- LET EXPRESSION


let_ : SyntaxVersion -> P.Parser Src.Expr
let_ syntaxVersion =
    P.inContext (P.CtxNode P.NLet)
        (P.succeed ()
            |. Keyword.let_ (P.Problem_Expr (P.EP_Let P.LP_Space))
            |> P.andThen (\_ ->
                P.withBacksetIndent 3
                    (Space.chomp
                        |> P.andThen (\_ ->
                            P.withIndent
                                (chompLetDef syntaxVersion
                                    |> P.andThen (\def -> chompLetDefs syntaxVersion [ def ])
                                )
                           )
                    )
                    |> P.andThen (\defs ->
                        Space.chomp
                            |> P.andThen (\_ ->
                                P.succeed ()
                                    |. Keyword.in_ (P.Problem_Expr (P.EP_Let P.LP_In))
                                    |> P.andThen (\_ ->
                                        Space.chomp
                                            |> P.andThen (\_ ->
                                                expression syntaxVersion
                                                    |> P.map (\body ->
                                                        A.merge (List.head defs |> Maybe.withDefault (A.sameAs body "")) body (Src.Let defs body)
                                                       )
                                               )
                                       )
                               )
                       )
               )
        )


chompLetDefs : SyntaxVersion -> List (A.Located Src.Def) -> P.Parser (List (A.Located Src.Def))
chompLetDefs syntaxVersion revDefs =
    P.oneOfWithFallback
        [ Space.checkAligned (P.Problem_Expr (P.EP_Let (P.LP_DefAlignment 0))) -- dummy alignment
            |> P.andThen (\_ ->
                chompLetDef syntaxVersion
                    |> P.andThen (\def -> chompLetDefs syntaxVersion (def :: revDefs))
               )
        ]
        (List.reverse revDefs)



-- LET DEFINITIONS


chompLetDef : SyntaxVersion -> P.Parser (A.Located Src.Def)
chompLetDef syntaxVersion =
    P.oneOf
        [ definition syntaxVersion
        , destructure syntaxVersion
        ]



-- DEFINITION


definition : SyntaxVersion -> P.Parser (A.Located Src.Def)
definition syntaxVersion =
    P.addLocation (Var.lower (P.Problem_Expr (P.EP_Let P.LP_DefName)))
        |> P.andThen (\name ->
            Space.chomp
                |> P.andThen (\_ ->
                    P.oneOf
                        [ P.succeed ()
                            |. P.word1 ':' (P.Problem_Expr (P.EP_Let (P.LP_Def (A.toValue name) P.DP_Equals)))
                            |> P.andThen (\_ ->
                                Space.chomp
                                    |> P.andThen (\_ ->
                                        Type.expression
                                            |> P.andThen (\tipe ->
                                                Space.chomp
                                                    |> P.andThen (\_ -> chompMatchingName (A.toValue name))
                                                    |> P.andThen (\defName ->
                                                        Space.chomp
                                                            |> P.andThen (\_ -> chompDefArgsAndBody syntaxVersion defName (Just tipe) [])
                                                       )
                                               )
                                       )
                               )
                        , chompDefArgsAndBody syntaxVersion name Nothing []
                        ]
                   )
           )


chompDefArgsAndBody : SyntaxVersion -> A.Located Name.Name -> Maybe Src.Type -> List Src.Pattern -> P.Parser (A.Located Src.Def)
chompDefArgsAndBody syntaxVersion name tipe revArgs =
    P.oneOf
        [ Pattern.term syntaxVersion
            |> P.andThen (\arg ->
                Space.chomp
                    |> P.andThen (\_ -> chompDefArgsAndBody syntaxVersion name tipe (arg :: revArgs))
               )
        , P.succeed ()
            |. P.word1 '=' (P.Problem_Expr (P.EP_Let (P.LP_Def (A.toValue name) P.DP_Equals)))
            |> P.andThen (\_ ->
                Space.chomp
                    |> P.andThen (\_ ->
                        expression syntaxVersion
                            |> P.map (\body ->
                                A.merge name body (Src.Define name (List.reverse revArgs) body tipe)
                               )
                       )
               )
        ]


chompMatchingName : Name.Name -> P.Parser (A.Located Name.Name)
chompMatchingName expectedName =
    P.addLocation (Var.lower (P.Problem_Expr (P.EP_Let (P.LP_Def expectedName P.DP_NameRepeat))))
        |> P.andThen (\locatedName ->
            if A.toValue locatedName == expectedName then
                P.succeed locatedName
            else
                P.problem (P.Problem_Expr (P.EP_Let (P.LP_Def expectedName (P.DP_NameMatch (A.toValue locatedName)))))
           )



-- DESTRUCTURE


destructure : SyntaxVersion -> P.Parser (A.Located Src.Def)
destructure syntaxVersion =
    P.addLocation
        (Pattern.term syntaxVersion
            |> P.andThen (\pattern ->
                Space.chomp
                    |> P.andThen (\_ ->
                        P.succeed ()
                            |. P.word1 '=' (P.Problem_Expr (P.EP_Let (P.LP_Destruct P.DDP_Equals)))
                            |> P.andThen (\_ ->
                                Space.chomp
                                    |> P.andThen (\_ ->
                                        expression syntaxVersion
                                            |> P.map (\expr -> Src.Destruct pattern expr)
                                       )
                               )
                       )
               )
        )

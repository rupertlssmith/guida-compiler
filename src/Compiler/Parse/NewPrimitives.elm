module Compiler.Parse.NewPrimitives exposing
    ( Parser
    , Row
    , Col
    , Step(..)
    , Snippet(..)
    , fmap
    , pure
    , bind
    , oneOf
    , oneOfWithFallback
    , loop
    , fromByteString
    , fromSnippet
    , getPosition
    , addLocation
    , addEnd
    , withIndent
    , withBacksetIndent
    , inContext
    , word1
    , word2
    , keyword
    , variable
    , number
    , unsafeIndex
    , isWord
    , getCharWidth
    , snippetEncoder
    , snippetDecoder
    , Context(..)
    , Node(..)
    , Problem(..)
    , ExprProblem(..)
    , RecordProblem(..)
    , StringProblem(..)
    , CharProblem(..)
    , EscapeProblem(..)
    , NumberProblem(..)
    , SpaceProblem(..)
    , TypeProblem(..)
    , TRecordProblem(..)
    , TTupleProblem(..)
    , PatternProblem(..)
    , PRecordProblem(..)
    , PTupleProblem(..)
    , PListProblem(..)
    , DeclProblem(..)
    , DeclDefProblem(..)
    , PortProblem(..)
    , DeclTypeProblem(..)
    , TypeAliasProblem(..)
    , CustomTypeProblem(..)
    , ModuleProblem(..)
    , ExposingProblem(..)
    )

import Compiler.Data.Name exposing (Name)
import Compiler.Parse.Symbol as Symbol
import Compiler.Reporting.Annotation as A
import Parser.Advanced as P
import Set
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Crash exposing (crash)


type alias Parser a =
    P.Parser Context Problem a


type alias Row =
    Int


type alias Col =
    Int


type alias Step =
    P.Step


type Context
    = CtxNode Node
    | CtxDef Name
    | CtxDestruct


type Node
    = NRecord
    | NParens
    | NList
    | NFunc
    | NCond
    | NThen
    | NElse
    | NCase
    | NBranch


type Problem
    = Problem_Expr ExprProblem
    | Problem_Record RecordProblem
    | Problem_String StringProblem
    | Problem_Char CharProblem
    | Problem_Number NumberProblem
    | Problem_Space SpaceProblem
    | Problem_Type TypeProblem
    | Problem_Pattern PatternProblem
    | Problem_Decl DeclProblem
    | Problem_Module ModuleProblem

type ModuleProblem
    = MP_Space
    | MP_BadEnd
    | MP_Problem
    | MP_ModuleName
    | MP_Exposing ExposingProblem
    | MP_PortModuleProblem
    | MP_PortModuleName
    | MP_PortModuleExposing ExposingProblem
    | MP_Effect
    | MP_FreshLine
    | MP_ImportStart
    | MP_ImportName
    | MP_ImportAs
    | MP_ImportAlias
    | MP_ImportExposing
    | MP_ImportExposingList ExposingProblem
    | MP_ImportEnd
    | MP_ImportIndentName
    | MP_ImportIndentAlias
    | MP_ImportIndentExposingList
    | MP_Infix
    | MP_Declarations

type ExposingProblem
    = EP_Space
    | EP_Start
    | EP_Value
    | EP_Operator
    | EP_OperatorReserved Symbol.BadOperator
    | EP_OperatorRightParen
    | EP_TypePrivacy
    | EP_End
    | EP_IndentEnd
    | EP_IndentValue

type DeclProblem
    = DP_Start
    | DP_Space SpaceProblem
    | DP_Port PortProblem
    | DP_Type DeclTypeProblem
    | DP_Def Name DeclDefProblem
    | DP_FreshLineAfterDocComment

type DeclDefProblem
    = DDP_Space
    | DDP_Equals
    | DDP_Type
    | DDP_Arg
    | DDP_Body
    | DDP_NameRepeat
    | DDP_NameMatch Name
    | DDP_IndentType
    | DDP_IndentEquals
    | DDP_IndentBody

type PortProblem
    = PP_Space
    | PP_Name
    | PP_Colon
    | PP_Type
    | PP_IndentName
    | PP_IndentColon
    | PP_IndentType

type DeclTypeProblem
    = DTP_Space
    | DTP_Name
    | DTP_Alias TypeAliasProblem
    | DTP_Union CustomTypeProblem
    | DTP_IndentName

type TypeAliasProblem
    = TAP_Space
    | TAP_Name
    | TAP_Equals
    | TAP_Body
    | TAP_IndentEquals
    | TAP_IndentBody

type CustomTypeProblem
    = CTP_Space
    | CTP_Name
    | CTP_Equals
    | CTP_Bar
    | CTP_Variant
    | CTP_VariantArg
    | CTP_IndentEquals
    | CTP_IndentBar
    | CTP_IndentAfterBar
    | CTP_IndentAfterEquals

type PatternProblem
    = PP_Record PRecordProblem
    | PP_Tuple PTupleProblem
    | PP_List PListProblem
    | PP_Start
    | PP_Char CharProblem
    | PP_String StringProblem
    | PP_Number NumberProblem
    | PP_Float Int
    | PP_Alias
    | PP_WildcardNotVar Name Int
    | PP_WildcardReservedWord Name Int
    | PP_Space SpaceProblem
    | PP_IndentStart
    | PP_IndentAlias

type PRecordProblem
    = PRP_Open
    | PRP_End
    | PRP_Field
    | PRP_Space SpaceProblem
    | PRP_IndentOpen
    | PRP_IndentEnd
    | PRP_IndentField

type PTupleProblem
    = PTP_Open
    | PTP_End
    | PTP_Expr PatternProblem
    | PTP_Space SpaceProblem
    | PTP_IndentEnd
    | PTP_IndentExpr1
    | PTP_IndentExprN

type PListProblem
    = PLP_Open
    | PLP_End
    | PLP_Expr PatternProblem
    | PLP_Space SpaceProblem
    | PLP_IndentOpen
    | PLP_IndentEnd
    | PLP_IndentExpr

type TypeProblem
    = TP_Record TRecordProblem
    | TP_Tuple TTupleProblem
    | TP_Start
    | TP_Space SpaceProblem
    | TP_IndentStart

type TRecordProblem
    = TRP_Open
    | TRP_End
    | TRP_Field
    | TRP_Colon
    | TRP_Type TypeProblem
    | TRP_Space SpaceProblem
    | TRP_IndentOpen
    | TRP_IndentField
    | TRP_IndentColon
    | TRP_IndentType
    | TRP_IndentEnd

type TTupleProblem
    = TTP_Open
    | TTP_End
    | TTP_Type TypeProblem
    | TTP_Space SpaceProblem
    | TTP_IndentType1
    | TTP_IndentTypeN
    | TTP_IndentEnd


type ExprProblem
    = EP_Let LetProblem
    | EP_Case CaseProblem
    | EP_If IfProblem
    | EP_List ListProblem
    | EP_Record RecordProblem
    | EP_Tuple TupleProblem
    | EP_Func FuncProblem
    | EP_Dot
    | EP_Access
    | EP_OperatorRight Name
    | EP_OperatorReserved Symbol.BadOperator
    | EP_Start
    | EP_Char CharProblem
    | EP_String StringProblem
    | EP_Number NumberProblem
    | EP_Space SpaceProblem
    | EP_EndlessShader
    | EP_ShaderProblem String
    | EP_IndentOperatorRight Name


type RecordProblem
    = RP_Open
    | RP_End
    | RP_Field
    | RP_Equals
    | RP_Expr ExprProblem
    | RP_Space SpaceProblem
    | RP_IndentOpen
    | RP_IndentEnd
    | RP_IndentField
    | RP_IndentEquals
    | RP_IndentExpr

type ListProblem
    = LP_Space
    | LP_Open
    | LP_Expr
    | LP_End
    | LP_IndentOpen
    | LP_IndentEnd
    | LP_IndentExpr

type TupleProblem
    = TUP_Expr
    | TUP_Space
    | TUP_End
    | TUP_OperatorClose
    | TUP_OperatorReserved Symbol.BadOperator
    | TUP_IndentExpr1
    | TUP_IndentExprN
    | TUP_IndentEnd

type FuncProblem
    = FP_Space
    | FP_Arg
    | FP_Body
    | FP_Arrow
    | FP_IndentArg
    | FP_IndentArrow
    | FP_IndentBody

type CaseProblem
    = CP_Space
    | CP_Of
    | CP_Pattern
    | CP_Arrow
    | CP_Expr
    | CP_Branch
    | CP_IndentOf
    | CP_IndentExpr
    | CP_IndentPattern
    | CP_IndentArrow
    | CP_IndentBranch
    | CP_PatternAlignment Int

type IfProblem
    = IP_Space
    | IP_Then
    | IP_Else
    | IP_ElseBranchStart
    | IP_Condition
    | IP_ThenBranch
    | IP_ElseBranch
    | IP_IndentCondition
    | IP_IndentThen
    | IP_IndentThenBranch
    | IP_IndentElseBranch
    | IP_IndentElse

type LetProblem
    = LP_Space
    | LP_In
    | LP_DefAlignment Int
    | LP_DefName
    | LP_Def Name DefProblem
    | LP_Destruct DestructProblem
    | LP_Body
    | LP_IndentDef
    | LP_IndentIn
    | LP_IndentBody

type DefProblem
    = DP_Space
    | DP_Type
    | DP_NameRepeat
    | DP_NameMatch Name
    | DP_Arg
    | DP_Equals
    | DP_Body
    | DP_IndentEquals
    | DP_IndentType
    | DP_IndentBody
    | DP_Alignment Int

type DestructProblem
    = DDP_Space
    | DDP_Pattern
    | DDP_Equals
    | DDP_Body
    | DDP_IndentEquals
    | DDP_IndentBody


type StringProblem
    = SP_Endless_Single
    | SP_Endless_Multi
    | SP_Escape EscapeProblem


type CharProblem
    = CP_Endless
    | CP_Escape EscapeProblem
    | CP_NotString Int


type EscapeProblem
    = EP_Unknown
    | EP_BadUnicodeFormat Int
    | EP_BadUnicodeCode Int
    | EP_BadUnicodeLength Int Int Int


type NumberProblem
    = NP_End
    | NP_Dot Int
    | NP_HexDigit
    | NP_NoLeadingZero


type SpaceProblem
    = HasTab
    | EndlessMultiComment



-- FUNCTOR


fmap : (a -> b) -> Parser a -> Parser b
fmap =
    P.map



-- MONAD


pure : a -> Parser a
pure =
    P.succeed


bind : (a -> Parser b) -> Parser a -> Parser b
bind callback parser =
    P.andThen callback parser



-- COMBINATORS


oneOf : List (Parser a) -> Parser a
oneOf =
    P.oneOf


oneOfWithFallback : List (Parser a) -> a -> Parser a
oneOfWithFallback parsers fallback =
    P.oneOf (parsers ++ [ P.succeed fallback ])


loop : (state -> Parser (P.Step state a)) -> state -> Parser a
loop callback state =
    P.loop state callback



-- RUN


fromByteString : Parser a -> String -> Result (List (P.DeadEnd Context Problem)) a
fromByteString =
    P.run


type Snippet
    = Snippet
        { fptr : String
        , offset : Int
        , length : Int
        , offRow : Row
        , offCol : Col
        }


fromSnippet : Parser a -> Snippet -> Result (List (P.DeadEnd Context Problem)) a
fromSnippet parser (Snippet { fptr, offset, length, offRow, offCol }) =
    let
        substring =
            String.slice offset (offset + length) fptr
    in
    -- This is not quite right because the row/col will be off.
    -- elm/parser does not seem to support starting parsing from a given row/col.
    -- For now, this is the best I can do.
    P.run parser substring



-- POSITION


getPosition : Parser A.Position
getPosition =
    P.getPosition
        |> P.map (\( row, col ) -> A.Position row col)


addLocation : Parser a -> Parser (A.Located a)
addLocation parser =
    P.succeed A.at
        |= getPosition
        |= parser
        |= getPosition


addEnd : A.Position -> a -> Parser (A.Located a)
addEnd start value =
    P.succeed (\end -> A.at start end value)
        |= getPosition



-- INDENT


withIndent : Parser a -> Parser a
withIndent parser =
    P.getCol
        |> P.andThen (\col -> P.withIndent col parser)


withBacksetIndent : Int -> Parser a -> Parser a
withBacksetIndent backset parser =
    P.getCol
        |> P.andThen (\col -> P.withIndent (col - backset) parser)



-- CONTEXT


inContext : Context -> Parser a -> Parser a
inContext =
    P.inContext



-- SYMBOLS


word1 : Char -> Problem -> Parser ()
word1 char problem =
    P.token (P.Token (String.fromChar char) problem)


word2 : Char -> Char -> Problem -> Parser ()
word2 c1 c2 problem =
    P.token (P.Token (String.fromChars [ c1, c2 ]) problem)


variable :
    { start : Char -> Bool
    , inner : Char -> Bool
    , reserved : Set.Set String
    , expecting : Problem
    }
    -> Parser String
variable =
    P.variable


keyword : String -> Problem -> Parser ()
keyword kw problem =
    P.keyword (P.Token kw problem)


number :
    { int : Result Problem (Int -> a)
    , hex : Result Problem (Int -> a)
    , octal : Result Problem (Int -> a)
    , binary : Result Problem (Int -> a)
    , float : Result Problem (Float -> a)
    , invalid : Problem
    , expecting : Problem
    }
    -> Parser a
number =
    P.number



-- LOW-LEVEL CHECKS


unsafeIndex : String -> Int -> Char
unsafeIndex str index =
    case String.uncons (String.dropLeft index str) of
        Just ( char, _ ) ->
            char

        Nothing ->
            crash "Error on unsafeIndex!"


isWord : String -> Int -> Int -> Char -> Bool
isWord src pos end word =
    pos < end && unsafeIndex src pos == word


getCharWidth : Char -> Int
getCharWidth word =
    if Char.toCode word > 0xFFFF then
        2

    else
        1



-- ENCODERS and DECODERS


snippetEncoder : Snippet -> BE.Encoder
snippetEncoder (Snippet { fptr, offset, length, offRow, offCol }) =
    BE.sequence
        [ BE.string fptr
        , BE.int offset
        , BE.int length
        , BE.int offRow
        , BE.int offCol
        ]


snippetDecoder : BD.Decoder Snippet
snippetDecoder =
    BD.map5
        (\fptr offset length offRow offCol ->
            Snippet
                { fptr = String.fromList (String.toList fptr)
                , offset = offset
                , length = length
                , offRow = offRow
                , offCol = offCol
                }
        )
        BD.string
        BD.int
        BD.int
        BD.int
        BD.int

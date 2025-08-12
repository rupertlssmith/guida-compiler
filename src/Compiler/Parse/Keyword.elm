module Compiler.Parse.Keyword exposing
    ( alias_
    , as_
    , case_
    , command_
    , effect_
    , else_
    , exposing_
    , if_
    , import_
    , in_
    , infix_
    , left_
    , let_
    , module_
    , non_
    , of_
    , port_
    , right_
    , subscription_
    , then_
    , type_
    , where_
    )

import Compiler.Parse.NewPrimitives as P


-- DECLARATIONS
type_ : P.Problem -> P.Parser ()
type_ problem =
    P.keyword "type" problem


alias_ : P.Problem -> P.Parser ()
alias_ problem =
    P.keyword "alias" problem


port_ : P.Problem -> P.Parser ()
port_ problem =
    P.keyword "port" problem



-- IF EXPRESSIONS
if_ : P.Problem -> P.Parser ()
if_ problem =
    P.keyword "if" problem


then_ : P.Problem -> P.Parser ()
then_ problem =
    P.keyword "then" problem


else_ : P.Problem -> P.Parser ()
else_ problem =
    P.keyword "else" problem



-- CASE EXPRESSIONS
case_ : P.Problem -> P.Parser ()
case_ problem =
    P.keyword "case" problem


of_ : P.Problem -> P.Parser ()
of_ problem =
    P.keyword "of" problem



-- LET EXPRESSIONS
let_ : P.Problem -> P.Parser ()
let_ problem =
    P.keyword "let" problem


in_ : P.Problem -> P.Parser ()
in_ problem =
    P.keyword "in" problem



-- INFIXES
infix_ : P.Problem -> P.Parser ()
infix_ problem =
    P.keyword "infix" problem


left_ : P.Problem -> P.Parser ()
left_ problem =
    P.keyword "left" problem


right_ : P.Problem -> P.Parser ()
right_ problem =
    P.keyword "right" problem


non_ : P.Problem -> P.Parser ()
non_ problem =
    P.keyword "non" problem



-- IMPORTS
module_ : P.Problem -> P.Parser ()
module_ problem =
    P.keyword "module" problem


import_ : P.Problem -> P.Parser ()
import_ problem =
    P.keyword "import" problem


exposing_ : P.Problem -> P.Parser ()
exposing_ problem =
    P.keyword "exposing" problem


as_ : P.Problem -> P.Parser ()
as_ problem =
    P.keyword "as" problem



-- EFFECTS
effect_ : P.Problem -> P.Parser ()
effect_ problem =
    P.keyword "effect" problem


where_ : P.Problem -> P.Parser ()
where_ problem =
    P.keyword "where" problem


command_ : P.Problem -> P.Parser ()
command_ problem =
    P.keyword "command" problem


subscription_ : P.Problem -> P.Parser ()
subscription_ problem =
    P.keyword "subscription" problem

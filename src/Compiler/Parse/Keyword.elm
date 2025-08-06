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
import Parser exposing (..)
import Parser.Advanced as Advanced


keyword : String -> (P.Row -> P.Col -> x) -> P.Parser x ()
keyword word toError =
    Advanced.keyword word (\_ r c -> toError r c)



-- DECLARATIONS


type_ : (P.Row -> P.Col -> x) -> P.Parser x ()
type_ =
    keyword "type"


alias_ : (P.Row -> P.Col -> x) -> P.Parser x ()
alias_ =
    keyword "alias"


port_ : (P.Row -> P.Col -> x) -> P.Parser x ()
port_ =
    keyword "port"



-- IF EXPRESSIONS


if_ : (P.Row -> P.Col -> x) -> P.Parser x ()
if_ =
    keyword "if"


then_ : (P.Row -> P.Col -> x) -> P.Parser x ()
then_ =
    keyword "then"


else_ : (P.Row -> P.Col -> x) -> P.Parser x ()
else_ =
    keyword "else"



-- CASE EXPRESSIONS


case_ : (P.Row -> P.Col -> x) -> P.Parser x ()
case_ =
    keyword "case"


of_ : (P.Row -> P.Col -> x) -> P.Parser x ()
of_ =
    keyword "of"



-- LET EXPRESSIONS


let_ : (P.Row -> P.Col -> x) -> P.Parser x ()
let_ =
    keyword "let"


in_ : (P.Row -> P.Col -> x) -> P.Parser x ()
in_ =
    keyword "in"



-- INFIXES


infix_ : (P.Row -> P.Col -> x) -> P.Parser x ()
infix_ =
    keyword "infix"


left_ : (P.Row -> P.Col -> x) -> P.Parser x ()
left_ =
    keyword "left"


right_ : (P.Row -> P.Col -> x) -> P.Parser x ()
right_ =
    keyword "right"


non_ : (P.Row -> P.Col -> x) -> P.Parser x ()
non_ =
    keyword "non"



-- IMPORTS


module_ : (P.Row -> P.Col -> x) -> P.Parser x ()
module_ =
    keyword "module"


import_ : (P.Row -> P.Col -> x) -> P.Parser x ()
import_ =
    keyword "import"


exposing_ : (P.Row -> P.Col -> x) -> P.Parser x ()
exposing_ =
    keyword "exposing"


as_ : (P.Row -> P.Col -> x) -> P.Parser x ()
as_ =
    keyword "as"



-- EFFECTS


effect_ : (P.Row -> P.Col -> x) -> P.Parser x ()
effect_ =
    keyword "effect"


where_ : (P.Row -> P.Col -> x) -> P.Parser x ()
where_ =
    keyword "where"


command_ : (P.Row -> P.Col -> x) -> P.Parser x ()
command_ =
    keyword "command"


subscription_ : (P.Row -> P.Col -> x) -> P.Parser x ()
subscription_ =
    keyword "subscription"

module Compiler.Reporting.Warning exposing
    ( Context(..)
    , Warning(..)
    )

import Compiler.AST.Canonical as Can
import Compiler.Data.Name exposing (Name)
import Compiler.Reporting.Annotation as A



-- ALL POSSIBLE WARNINGS


type Warning
    = UnusedImport A.Region Name
    | UnusedVariable A.Region Context Name
    | MissingTypeAnnotation A.Region Name Can.Type


type Context
    = Def
    | Pattern



-- TO REPORT

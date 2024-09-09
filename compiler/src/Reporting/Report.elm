module Reporting.Report exposing (Report(..))

import Reporting.Annotation as A
import Reporting.Doc as D



-- BUILD REPORTS


type Report
    = Report String A.Region (List String) D.Doc

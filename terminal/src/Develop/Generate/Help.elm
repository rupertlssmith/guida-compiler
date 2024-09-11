module Develop.Generate.Help exposing
    ( makeCodeHtml
    , makePageHtml
    )

import Data.Name as Name
import Json.EncodeX as Encode
import Utils.Main as Utils



-- PAGES


makePageHtml : Name.Name -> Maybe Encode.Value -> String
makePageHtml moduleName maybeFlags =
    """<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <link type="text/css" rel="stylesheet" href="/_elm/styles.css">
  <script src="/_elm/elm.js"></script>
</head>
<body>
<script>
Elm.""" ++ moduleName ++ """.init({ flags: """ ++ Utils.maybe "undefined" Encode.encode maybeFlags ++ """ });
</script>
</body>
</html>
"""



-- CODE


makeCodeHtml : String -> String -> String
makeCodeHtml title code =
    """<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <title>""" ++ title ++ """</title>
  <style type="text/css">
    @import url(/_elm/source-code-pro.ttf);
    html, head, body, pre { margin: 0; height: 100%; }
    body { font-family: "Source Code Pro", monospace; }
  </style>
  <link type="text/css" rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.3.0/styles/default.min.css">
  <script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.3.0/highlight.min.js"></script>
  <script>if (hljs) { hljs.initHighlightingOnLoad(); }</script>
</head>
<body style="background-color: #F0F0F0;">
<pre><code>""" ++ code ++ """</code></pre>
</body>
</html>
"""

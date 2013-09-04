module Testing (module Utils,
                module Decl.Document,
                module Decl.DocumentHref,
                module Grammars.HtmlHref,
                module Grammars.Markdown,
                module Semantics.Html,
                module Semantics.HtmlHref,
                module Semantics.NumberedHeaders,
                module Semantics.HtmlNumberedHeaders,
                module Preprocessors,
                module Language.Grammars.AspectAG,
                module Grammars.Latex) where



import Utils
import Decl.Document
import Decl.DocumentHref
import Grammars.Html
import Grammars.HtmlHref
import Grammars.Markdown
import Grammars.Latex
import Semantics.Html
import Semantics.HtmlHref
import Semantics.NumberedHeaders
import Semantics.HtmlNumberedHeaders
import Preprocessors

import Language.Grammars.Murder
import Language.Grammars.AspectAG
import Language.Grammars.Murder.UUParsing

html2html input = let  parser = compile $ closeGram (gHtml semHtml)
                       x      = result (parse parser input)
                   in  (x emptyRecord) # output
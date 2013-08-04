{-# LANGUAGE Arrows, RecursiveDo, EmptyDataDecls, TemplateHaskell #-}

module Grammars.HTML where

import Prelude hiding ((+), (*))
import Control.Applicative

import Language.Grammars.Grammar
import Language.Grammars.Murder
import Language.Grammars.Murder.Derive
import Language.Grammars.Murder.UUParsing

import qualified Document as D
import Utils

$(csLabels  ["cs_br", "cs_inline", "cs_inlines", "cs_paragraph", "cs_body", "cs_header"])

gHTML = proc () -> do
    rec 
        br        <- addNT -< tr "<br>" <|> tr "<br />"

        inline    <- addNT -<  iI semPlain   (pMany . sym . anyof $ "abc") Ii
                           <|> iI semBold    "<b>" inlines "</b>" Ii
                           <|> iI semItalics "<i>" inlines "</i>" Ii
        
        inlines   <- addNT -< pSome . sym $ inline
        
        paragraph <- addNT -< iI semParagraph inlines Ii

        body      <- addNT -< iI semBody (pMany . sym . anyof $ bodyChars) Ii 

        header    <- addNT -< iI semHeader "<h1>" body "</h1>" Ii

    exportNTs -<  exportList paragraph ( export cs_br     br
                                    . export cs_inline   inline
                                    . export cs_inlines  inlines
                                    . export cs_body   body
                                    . export cs_header header
                                    . export cs_paragraph paragraph)


bodyChars = ['a'..'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " ':;!@()\""


pHTML = compile (closeGram gHTML)


-- Semantics for building the AST
--semPlain :: DTerm String -> D.Inline
semPlain = D.Plain . map value

semBold :: [D.Inline] -> D.Inline
semBold = D.Bold

semItalics :: [D.Inline] -> D.Inline
semItalics = D.Italics


semParagraph :: [D.Inline] -> D.Block
semParagraph = D.Paragraph

semBody :: [DTerm Char] -> String
semBody = map value

semHeader :: String -> D.Block
semHeader body = D.Header 1 [(D.Plain body)]
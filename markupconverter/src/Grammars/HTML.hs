{-# LANGUAGE Arrows, RecursiveDo, EmptyDataDecls, TemplateHaskell, PostfixOperators #-}

module Grammars.HTML where

import Prelude hiding ((+), (*))
import Data.Char
import Control.Applicative

import Language.Grammars.Grammar
import Language.Grammars.Murder
import Language.Grammars.Murder.Derive
import Language.Grammars.Murder.UUParsing

import qualified Document as D
import Utils

$(csLabels  ["cs_br", "cs_inline", "cs_inlines", "cs_paragraph", "cs_body", "cs_header"])

-- manyExcept :: String -> PreProductions l String env
someExcept cs = pSome $ iI value (sym $ anyexcept cs) Ii

gHTML = proc () -> do
    rec 
        root      <-addNT-< iI (D.Root . D.Document) blocks Ii
        
        blocks    <-addNT-< pMany $ (iI header Ii) <|> (iI paragraph Ii)
        
        paragraph <-addNT-< iI semParagraph "<p>" inlines "</p>" Ii
        
        header    <-addNT-< iI semHeader    "<h" (foldr1 (<|>) $ map tr ["1","2", "3","4","5","6"]) ">" inlines "</h" (sym $ anyof "123456") ">" Ii
        

        inlineP   <-addNT-<  iI semPlain (someExcept "<") Ii
        inlineT   <-addNT-<  iI semBold    "<b>" inlines "</b>" Ii
                         <|> iI semItalics "<i>" inlines "</i>" Ii
        
        inlines   <-addNT-< iI semInlinesSingle  inlineP                   Ii
                        <|> iI semInlinesSeq    (inlineP?) inlineT inlines Ii
                        <|> pure []
        
        -- text      <-addNT-< iI  Ii
        

        


    exportNTs -<  exportList root ( {- export cs_br     br 
                                    .-} {- export cs_inline   inline
                                    .-} export cs_inlines  inlines
                                    {- . export cs_body   body
                                    . export cs_header header
                                    . export cs_paragraph paragraph -})



pHTML = compile (closeGram gHTML)


-- Semantics for building the AST
--semPlain :: Maybe String -> D.Inline ->D.Inline
semPlain = D.Plain

semInlinesSingle = (: [])

semInlinesSeq (Just pl) t is = pl:t:is
semInlinesSeq _         t is = t:is

semInlinesEmpty = id

semBold :: [D.Inline] -> D.Inline
semBold = D.Bold

semItalics :: [D.Inline] -> D.Inline
semItalics = D.Italics


semParagraph :: [D.Inline] -> D.Block
semParagraph = D.Paragraph

semBody :: [DTerm Char] -> String
semBody = map value

semHeader :: DTerm String -> D.InlineL -> DTerm Char -> D.Block
semHeader level inlines _ = D.Header (read . value $ level) inlines
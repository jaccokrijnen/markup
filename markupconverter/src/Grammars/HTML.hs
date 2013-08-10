{-# LANGUAGE Arrows, RecursiveDo, EmptyDataDecls, TemplateHaskell, PostfixOperators, FlexibleContexts #-}

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

tag :: String -> PreProductions l env (a -> a)
tag x = iI "<" x ">" Ii



headerLvl body x = iI (semHeader' x) ("<h" ++ show x ++ ">") body ("</h" ++ show x ++ ">") Ii



semHeader <$> pIets <*> pIetsAnders


gHTML = proc () -> do
    rec 
        root      <-addNT-< iI D.Document blocks Ii
        
        blocks    <-addNT-< pMany $ (iI header Ii) <|> (iI paragraph Ii)
        
        paragraph <-addNT-< iI semParagraph "<p>" inlines "</p>" Ii
        
        header    <-addNT-< foldr1 (<|>) $ map (headerLvl inlines) [1..6]

        -- inline (plain) and inline (tag)
        inlineP   <-addNT-<  iI semPlain   (someExcept "<")     Ii
        inlineT   <-addNT-<  iI semBold    "<b>" inlines "</b>" Ii
                         <|> iI semItalics "<i>" inlines "</i>" Ii
        
        -- Multiple inlines, pMany does not suffice, since we cannot have two
        -- consecutive plain inlines (that would be ambiguous)
        inlines   <-addNT-<  iI semInlinesSingle  inlineP                   Ii
                         <|> iI semInlinesSeq    (inlineP?) inlineT inlines Ii
                         <|> pure []
        

        


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

semHeader' = D.Header
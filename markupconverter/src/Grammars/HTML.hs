{-# LANGUAGE Arrows, RecursiveDo, EmptyDataDecls, TemplateHaskell, PostfixOperators, FlexibleContexts #-}

module Grammars.HTML where

import Prelude hiding ((+), (*))
import Data.Char
import Control.Applicative

import Language.Grammars.Grammar
import Language.Grammars.Murder
import Language.Grammars.Murder.Derive
import Language.Grammars.Murder.UUParsing

import Decl.Document
import Utils

$(csLabels  ["cs_root", "cs_blocks", "cs_paragraph", "cs_header", "cs_inlineP", "cs_inlineT", "cs_inlines"])

-- manyExcept :: String -> PreProductions l String env
someExcept cs = pSome $ iI value (sym $ anyexcept cs) Ii



tag :: String -> Ign (PreProductions l env (a -> a))
tag x = ign $ iI "<" x ">" Ii


headerLvl :: (Int -> InlineL -> a) -> Symbol InlineL TNonT env -> Int -> PreProductions l env a
headerLvl pHeader body x = let open  = tag ("h"  ++ show (x :: Int))
                               close = tag ("/h" ++ show (x :: Int))
                           in  iI (pHeader x) open body close Ii


gHTML sem = proc () -> do
    rec 
        root      <-addNT-< iI (pDocument sem) blocks Ii
        
        
        paragraph <-addNT-< iI (pParagraph sem) (tag "p") inlines (tag "/p") Ii
        header    <-addNT-< foldr1 (<|>) $ 
                                map (headerLvl (pHeader sem) inlines) [1..6]
        blocks    <-addNT-< pMany $ (iI header Ii) <|> (iI paragraph Ii)
        
        -- inline (plain) and inline (tag)
        inlineP   <-addNT-<  iI (pPlain   sem) (someExcept "<")     Ii
        inlineT   <-addNT-<  iI (pBold    sem) (tag "b") inlines (tag "/b") Ii
                         <|> iI (pItalics sem) (tag "i") inlines (tag "/i") Ii
        
        -- Multiple inlines, pMany does not suffice, since we cannot have two
        -- consecutive plain inlines (that would be ambiguous)
        inlines   <-addNT-<  iI semInlinesSingle  inlineP                   Ii
                         <|> iI semInlinesSeq    (inlineP?) inlineT inlines Ii
                         <|> pure []
        

        


    exportNTs -<  exportList root (   export cs_root      root
                                    . export cs_blocks    blocks
                                    . export cs_paragraph paragraph
                                    . export cs_header    header
                                    . export cs_inlineP   inlineP
                                    . export cs_inlineT   inlineT
                                    . export cs_inlines   inlines)



pHTML = compile (closeGram (gHTML undefined))


-- Semantics for building the AST
--semPlain :: Maybe String -> Inline ->Inline
semPlain = Plain

semInlinesSingle = (: [])

semInlinesSeq (Just pl) t is = pl:t:is
semInlinesSeq _         t is = t:is

semInlinesEmpty = id

semBold :: [Inline] -> Inline
semBold = Bold

semItalics :: [Inline] -> Inline
semItalics = Italics


semParagraph :: [Inline] -> Block
semParagraph = Paragraph

semBody :: [DTerm Char] -> String
semBody = map value

semHeader :: DTerm String -> InlineL -> DTerm Char -> Block
semHeader level inlines _ = Header (read . value $ level) inlines

semHeader' = Header
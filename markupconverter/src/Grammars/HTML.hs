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

-- Generate the labels used as lookup keys in the exportlist
$(csLabels  ["cs_root", "cs_blocks", "cs_paragraph", "cs_header", "cs_inlinePlain", "cs_inlineTag", "cs_inlines"])



-- | Recognizes (but ignores) a tag, i.e. "<html>"
tag :: String -> Ign (PreProductions l env (DTerm String))
tag x = ign $ iI "<" (tr x) ">" Ii


-- | Recognizes a header at level x, i.e. "<hx> ... </hx>" 
headerLvl :: (Int -> InlineL -> a)    -- ^ The semantic function 
          -> Symbol InlineL TNonT env -- ^ The non terminal to be recognized between the tags
          -> Int                      -- ^ The level
          -> PreProductions l env a
headerLvl pHeader body x = let open  = tag ("h"  ++ show (x :: Int))
                               close = tag ("/h" ++ show (x :: Int))
                           in  iI (pHeader x) open body close Ii





-- | The grammar for simplified version of HTML
gHTML sem = proc () -> do
    rec 
        root         <-addNT-< iI (pDocument sem) blocks Ii
        
        
        blocks       <-addNT-< pMany $ (iI header Ii) <|> (iI paragraph Ii)
        paragraph    <-addNT-< iI (pParagraph sem) (tag "p") inlines (tag "/p") Ii
        header       <-addNT-< foldr1 (<|>) $ 
                                   map (headerLvl (pHeader sem) inlines) [1..6]
        
        -- this seperation is required for the inlines non-terminal
        inlinePlain  <-addNT-<  iI (pPlain   sem) (someExcept "<")     Ii
        inlineTag    <-addNT-<  iI (pBold    sem) (tag "b") inlines (tag "/b") Ii
                        <|>     iI (pItalics sem) (tag "i") inlines (tag "/i") Ii
        
        -- Multiple inlines, pMany does not suffice, since we cannot have two
        -- consecutive plain inlines (that would be ambiguous)
        inlines      <-addNT-<  iI semInlinesSingle  inlinePlain                 Ii
                        <|>     iI semInlinesSeq    (inlinePlain?) inlineTag inlines Ii
                        <|>     pure []
        

        


    exportNTs -<  exportList root (   export cs_root          root
                                    . export cs_blocks        blocks
                                    . export cs_paragraph     paragraph
                                    . export cs_header        header
                                    . export cs_inlinePlain   inlinePlain
                                    . export cs_inlineTag     inlineTag
                                    . export cs_inlines       inlines)



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
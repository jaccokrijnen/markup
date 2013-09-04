{-# LANGUAGE Arrows, RecursiveDo, EmptyDataDecls, TemplateHaskell, PostfixOperators, FlexibleContexts #-}

module Grammars.Html where

import Prelude hiding ((+), (*))
import Data.Char
import Control.Applicative

import Language.Grammars.Grammar
import Language.Grammars.Murder
import Language.Grammars.Murder.Derive
import Language.Grammars.Murder.UUParsing
import Language.Grammars.AspectAG
import Decl.Document
import Utils

-- Generate the labels used as lookup keys in the exportlist
$(csLabels  ["cs_document", "cs_blockL", "cs_paragraph", "cs_header", "cs_inline", "cs_inlineL"])

{-
-- | Recognizes a header at level x, i.e. "<hx> ... </hx>" 
headerLvl :: (int -> inlines -> a)    -- ^ The semantic function 
          -> Symbol inlines TNonT env -- ^ The non terminal to be recognized between the tags
          -> Int                      -- ^ The level
          -> PreProductions l env a -}
headerLvl pHeader body x = let open  = "<h"  ++ show x ++ ">"
                               close = "</h" ++ show x ++ ">"
                           in  iI (pHeader (sem_Lit x)) open body close Ii





-- | The grammar for a simplified version of Html
gHtml sem = proc () -> do
    rec 
        document     <-addNT-< iI (pDocument sem) blockL Ii
        
        
        blockL       <-addNT-< pFoldr (pBlockL_Cons sem, pBlockL_Nil sem) $ 
                                   (iI header Ii) <|> (iI paragraph Ii)
        paragraph    <-addNT-< iI (pParagraph sem) "<p>" inlineL "</p>" Ii
        header       <-addNT-< foldr1 (<|>) $ 
                                   map (headerLvl (pHeader sem) inlineL) [1..6]
        
        -- this seperation is required for the inlines non-terminal
        inline       <-addNT-<  iI (pPlain   sem . sem_Lit) "<plain>" (someExcept "<") "</plain>" Ii 
                        <|>     iI (pBold    sem) "<b>"     inlineL          "</b>"     Ii
                        <|>     iI (pItalics sem) "<i>"     inlineL          "</i>"     Ii
        
        -- Multiple inlines
        inlineL      <-addNT-<  pFoldr (pInlineL_Cons sem, pInlineL_Nil sem) $
                                    iI inline Ii
        

        


    exportNTs -<  exportList document (   export cs_document      document
                                        . export cs_blockL        blockL
                                        . export cs_paragraph     paragraph
                                        . export cs_header        header
                                        . export cs_inline        inline
                                        . export cs_inlineL       inlineL)

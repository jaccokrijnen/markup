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
$(csLabels  ["cs_root", "cs_blockL", "cs_paragraph", "cs_header", "cs_inline", "cs_inlineL"])



-- | Recognizes (but ignores) a tag, i.e. "<html>"
tag :: String -> Ign (PreProductions l env (DTerm String))
tag x = ign $ iI (tr $ "<" ++ x ++ ">") Ii


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
        root         <-addNT-< iI (pDocument sem) blockL Ii
        
        
        blockL       <-addNT-< pFoldr (pBlockL_Cons sem, pBlockL_Nil sem) $ 
                                   (iI header Ii) <|> (iI paragraph Ii)
        paragraph    <-addNT-< iI (pParagraph sem) (tag "p") inlineL (tag "/p") Ii
        header       <-addNT-< foldr1 (<|>) $ 
                                   map (headerLvl (pHeader sem) inlineL) [1..6]
        
        -- this seperation is required for the inlines non-terminal
        inline       <-addNT-<  iI (pPlain   sem) (tag "plain") (someExcept "<") (tag "/plain") Ii 
                        <|>     iI (pBold    sem) (tag "b")     inlineL          (tag "/b") Ii
                        <|>     iI (pItalics sem) (tag "i")     inlineL          (tag "/i") Ii
        
        -- Multiple inlines, pMany does not suffice, since we cannot have two
        -- consecutive plain inlines (that would be ambiguous)
        inlineL      <-addNT-<  pFoldr (pInlineL_Cons sem, pInlineL_Nil sem) $
                                    iI inline Ii
        

        


    exportNTs -<  exportList root (   export cs_root          root
                                    . export cs_blockL        blockL
                                    . export cs_paragraph     paragraph
                                    . export cs_header        header
                                    . export cs_inline        inline
                                    . export cs_inlineL       inlineL)



pHTML = compile (closeGram (gHTML semAst))


-- Semantics for building the AST

semAst = DocSF {
    pBlockL_Cons  = (:),
    pBlockL_Nil   = [],
    pBold         = Bold,
    pDocument     = Document,
    pHeader       = Header,
    pInlineL_Cons = (:),
    pInlineL_Nil  = [],
    pItalics      = Italics,
    pParagraph    = Paragraph,
    pPlain        = Plain
}
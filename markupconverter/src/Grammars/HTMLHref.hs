{-# LANGUAGE Arrows #-}

module Grammars.HTMLHref where

import Language.Grammars.Grammar
import Language.Grammars.Murder

import Grammars.HTML
import Decl.Document
import Decl.DocumentHref

import Utils


gHTMLHref sem = proc imported -> do
    let inline = getNT cs_inline imported
    
    _ <-addProds-< (inline,  
    				iI (pHref sem) "<a href=\"" (someExcept "\"") ">" (someExcept "<") "</a>" Ii)
    
    exportNTs -< imported
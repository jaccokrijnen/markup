{-# LANGUAGE Arrows #-}

module Grammars.HTMLHref where

import Language.Grammars.Grammar
import Language.Grammars.Murder

import Grammars.HTML
import Decl.Document
import Decl.DocumentHref

import Utils


gHTMLHref sem = proc imported -> do
    let inlineT = getNT cs_inlineT imported
    
    addProds-< (inlineT,  iI (pHref sem) "<a href=\"" (someExcept "\"") ">" (someExcept "<") "</a>" Ii)
    
    exportNTs -< imported
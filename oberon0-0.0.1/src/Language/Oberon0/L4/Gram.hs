{-# OPTIONS -fcontext-stack=100 #-}

{-# LANGUAGE  Arrows, DoRec #-}

module Language.Oberon0.L4.Gram (module Language.Oberon0.L4.Gram, module Language.Oberon0.L3.Gram) where

import Language.Oberon0.L3.Gram
import Language.Oberon0.L4.Decl

import Control.Arrow
import Control.Applicative
import Data.Set (union,fromList)

import Language.Grammars.Murder
import Language.Grammars.Grammar

import Language.Grammars.AspectAG


l4 sf = proc imported -> do
         let statement  = getNT cs_Statement          imported 
         let exp        = getNT cs_Expression         imported 
         let factor     = getNT cs_Factor             imported 
         let ident      = getNT cs_Ident              imported 
         let idl        = getNT cs_IdentL             imported 
         let typ        = getNT cs_Type               imported 


         rec  addProds   -<  (typ,     iI (pArrayType  sf) "ARRAY" exp "OF" typ Ii
                                 <|>   iI (pRecordType sf) "RECORD" fieldl "END" Ii)

              fieldl     <- addNT -<  iI (pFieldL_Cons sf) field (pFoldr (pFieldL_Cons sf, pFieldL_Nil sf) (iI ";" field Ii)) Ii

              field      <- addNT -<  iI (pField sf) idl ":" typ Ii <|> iI (pEmptyField sf) Ii


              addProds   -<  (factor,  iI (pSelExp  sf) ident selector Ii)


              selector   <- addNT -<  iI (pSelectL_Cons sf) sel (pFoldr (pSelectL_Cons sf, pSelectL_Nil sf) (iI sel Ii)) Ii

              sel        <- addNT -<  iI  (pSelField sf) "." ident Ii <|> iI (pSelArray sf)  "[" exp "]" Ii 

              addProds   -<  (statement,  iI (pAssigSelStmt  sf) ident selector ":=" exp Ii)

         exportNTs -< imported

l4Kws = union l3Kws $ fromList ["ARRAY","RECORD"]


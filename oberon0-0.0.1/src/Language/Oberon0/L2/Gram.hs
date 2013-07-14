{-# OPTIONS -fcontext-stack=100 #-}

{-# LANGUAGE  Arrows, DoRec #-}

module Language.Oberon0.L2.Gram (module Language.Oberon0.L2.Gram, module Language.Oberon0.L1.Gram) where

import Language.Oberon0.L1.Gram
import Language.Oberon0.L2.Decl

import Control.Arrow
import Control.Applicative
import Data.Set (union,fromList)

import Language.Grammars.Murder
import Language.Grammars.Grammar
import Language.Grammars.AspectAG


l2 sf = proc imported -> do
         let ss         = getNT cs_StatementSequence  imported 
         let statement  = getNT cs_Statement          imported 
         let exp        = getNT cs_Expression         imported 
         let ident      = getNT cs_Ident              imported 
         let mbelse     = getNT cs_MaybeElseStmt      imported 


         rec  addProds   -<  (statement,     iI (pForStmt  sf) "FOR" ident ":" "=" exp  dir exp  mbexp  "DO"  ss  "END" Ii <|>
                                             iI (pCaseStmt sf) "CASE" exp "OF" c cs mbelse "END" Ii)

              dir        <- addNT -<  iI (pTo sf) "TO" Ii <|> iI (pDownto sf) "DOWNTO" Ii
              mbexp      <- addNT -<  pMaybe (pCst1Exp sf, id) (iI "BY" exp Ii)


              cs         <- addNT -<  pFoldr (pCaseL_Cons sf, pCaseL_Nil sf) (iI "|" c Ii)
              c          <- addNT -<  iI (pCase sf) labels ":" ss Ii

              labels     <- addNT -<  iI (pLabelL_Cons sf) label (pFoldr (pLabelL_Cons sf, pLabelL_Nil sf) (iI "," label Ii)) Ii
              label      <- addNT -<  iI (pExpreLbl sf) exp  Ii <|> iI (pRangeLbl sf) exp ".." exp  Ii



         exportNTs -< imported

l2Kws = union l1Kws $ fromList ["FOR", "DO", "CASE", "OF", "TO", "DOWNTO", "BY"]


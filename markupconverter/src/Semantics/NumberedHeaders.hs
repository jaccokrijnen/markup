{-# LANGUAGE TemplateHaskell, EmptyDataDecls, NoMonomorphismRestriction #-}
module Semantics.NumberedHeaders where

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive

import Document


-- chained header number
$(attLabels ["cHeaderNum"])


semHeaderNum :: Document -> Int
semHeaderNum doc = sem_Document asp_cHeaderNum doc (cHeaderNum .=. 1 .*. emptyRecord) # cHeaderNum



asp_cHeaderNum =  (p_Document    .=. document_cHeaderNum)
              .*. (p_BlockL_Cons .=. blockLcons_cHeaderNum)
              .*. (p_BlockL_Nil  .=. blockLnil_cHeaderNum)
              .*. (p_Paragraph   .=. paragraph_cHeaderNum)
              .*. (p_Header      .=. header_cHeaderNum)
              .*. (p_InlineL_Nil  .=. cHeaderNumRule)
              .*. (p_InlineL_Cons .=. cHeaderNumRule)
              .*. (p_Plain        .=. cHeaderNumRule)
              .*. (p_Bold         .=. cHeaderNumRule) 
              .*. (p_Italics      .=. cHeaderNumRule)
              .*. emptyRecord


-- The general rule for the chained attribute
cHeaderNum_NTs = nt_Document .*. nt_BlockL .*. nt_Block .*. nt_Inline .*. nt_InlineL .*. hNil
cHeaderNumRule = chain cHeaderNum cHeaderNum_NTs

--------------------------
-- Document productions --
--------------------------
document_cHeaderNum = cHeaderNumRule {- inh cHeaderNum cHeaderNum_NTs $
                          return (  ch_blocks .=. (1 :: Int)
                                .*. emptyRecord) -}

------------------------
-- BlockL productions --
------------------------
blockLcons_cHeaderNum = cHeaderNumRule

blockLnil_cHeaderNum = cHeaderNumRule

-----------------------
-- Block productions --
-----------------------

paragraph_cHeaderNum = cHeaderNumRule

header_cHeaderNum = cHeaderNumRule
{- syn cHeaderNum $
                      do lhs <- at lhs
                         return ((lhs # cHeaderNum) + 1) -}
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



asp_cHeaderNum =  (p_Document    .=. cHeaderNumRule)
              .*. (p_BlockL_Cons .=. cHeaderNumRule)
              .*. (p_BlockL_Nil  .=. cHeaderNumRule)
              .*. (p_Paragraph   .=. cHeaderNumRule)
              .*. (p_Header      .=. header_cHeaderNum)
              .*. (p_InlineL_Nil  .=. emptyRule)
              .*. (p_InlineL_Cons .=. emptyRule)
              .*. (p_Plain        .=. emptyRule)
              .*. (p_Bold         .=. emptyRule) 
              .*. (p_Italics      .=. emptyRule)
              .*. emptyRecord


-- The general rule for the chained attribute
cHeaderNum_NTs = nt_Document .*. nt_BlockL .*. nt_Block .*. hNil
cHeaderNumRule = chain cHeaderNum cHeaderNum_NTs


-- Specific rule to update the header counter (if header level is 2)
header_cHeaderNum = syn cHeaderNum $
                        do lhs   <- at lhs
                           level <- at ch_level_header
                           return (if level == 2
                                      then (lhs # cHeaderNum) + 1
                                      else (lhs # cHeaderNum))
{-# LANGUAGE TemplateHaskell, EmptyDataDecls, NoMonomorphismRestriction #-}
module Semantics.NumberedHeaders where

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive

import Document


-- shNum: synthesized header number
-- ihNum: inherited header number
$(attLabels ["shNum", "ihNum"])


asp_hNum = asp_ihNum .+. asp_shNum


asp_ihNum =  (p_Document    .=. undefined) --document_ihNum)
         .*. (p_BlockL_Cons .=. blockLcons_ihNum)
         .*. emptyRecord

asp_shNum =  (p_Paragraph   .=. paragraph_shNum)
         .*. (p_Header      .=. header_shNum)
         .*. emptyRecord





-- Document productions

-- does not compile
{-
document_ihNum = inh ihNum (nt_Document .*. HNil) $
                          return (ch_blocks .=. 1
                                .*. emptyRecord)
-}


-- BlockL productions

-- Thread the Int to left child, thread (updated) Int to the rest

blockLcons_ihNum = chain ihNum (nt_BlockL) {- inh ihNum (nt_BlockL .*. HNil) $
                        do  lhs    <- at lhs
                            block  <- at ch_hd_BlockL_Cons
                            return (  ch_hd_BlockL_Cons .=. lhs # ihNum
                                  .*. ch_tl_BlockL_Cons .=. block # shNum
                                  .*. emptyRecord)
-}


-- Block productions 

paragraph_shNum = syn shNum $
                      do lhs    <- at lhs
                         return (lhs # ihNum)

header_shNum = syn shNum $
                      do lhs <- at lhs
                         return ((lhs # ihNum) + 1)
{-# LANGUAGE TemplateHaskell, EmptyDataDecls, NoMonomorphismRestriction #-}
module Semantics.HTML where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive

import Document

$(deriveAG ''Root)
$(attLabels ["shtml"])



-- example doc
root = Root (Document [Header 1  [Plain "A document"],
                       Paragraph [Plain "this is some text."],
                       Paragraph [Plain "mor text."]])

-- does not compile yet
-- result :: String
-- result = sem_ asp_shtml (Plain "test") () # shtml




------------------------------------------
-- Initial aspect shtml (synthesized html)
------------------------------------------

-- The aspect is a heterogenous list of pairs of production and rule
asp_shtml =  (p_Root      .=. root_shtml)
         .*. (p_Document  .=. document_shtml) 
         .*. (p_Header    .=. header_shtml) 
         .*. (p_Paragraph .=. paragraph_shtml)
         .*. (p_Plain     .=. plain_shtml)
         .*. (p_Bold      .=. bold_shtml) 
         .*. (p_Italics   .=. italics_shtml)
         .*. emptyRecord


--------------------------------
-- Rules for the attribute shtml
--------------------------------

-- Root production
root_shtml = syn shtml $
    do doc <- at ch_document
       return $ doc # shtml

-- Document production
document_shtml = syn shtml $
    do blocks <- at ch_blocks
       return $ concatMap (\b -> b # shtml) blocks



-- Block productions
header_shtml = syn shtml $
    do level <- at ch_level_head
       inls   <- at ch_inlines_head
       return $ "<h" ++ show level ++ ">" 
                 ++ (concatMap (# shtml) inls) 
                 ++ "</h" ++ show level ++ ">"

paragraph_shtml = syn shtml $
    do inls <- at ch_inlines_par
       return $ "<p>" 
              ++ (map (# shtml) inls)
              ++ "</p>"



-- Inline productions
plain_shtml = syn shtml $ at ch_str_plainInl

bold_shtml  = syn shtml $ 
                    do inls <- at ch_inlines_boldInl
                       return $ "<b>" 
                              ++ (concatMap (# shtml) inls) 
                              ++ "</b>"

italics_shtml  = syn shtml $ 
                    do inls <- at ch_inlines_italInl
                       return $ "<i>" 
                              ++ (concatMap (#shtml) inls) 
                              ++ "</i>"
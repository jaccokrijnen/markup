{-# LANGUAGE TemplateHaskell, EmptyDataDecls, NoMonomorphismRestriction #-}
module Semantics.Html where


import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive

import Decl.Document

$(attLabels ["shtml"])



-- example doc
doc = Document [Header 1  [Plain "A document"],
                Paragraph [Plain "this is some ", Bold [Plain "really ", Italics [Plain "important"]], Plain " text."],
                Paragraph [Plain "mor text."]]


semHtmlNotExtensible :: Document -> String
semHtmlNotExtensible doc = sem_Document asp_shtml doc () # shtml




------------------------------------------
-- Initial aspect shtml (synthesized html)
------------------------------------------

-- The aspect is a heterogenous list of pairs of production and rule
asp_shtml =  (p_Document     .=. document_shtml) 
         .*. (p_BlockL_Nil   .=. blockLnil_shtml)
         .*. (p_BlockL_Cons  .=. blockLcons_shtml)
         .*. (p_Header       .=. header_shtml) 
         .*. (p_Paragraph    .=. paragraph_shtml)
         .*. (p_InlineL_Nil  .=. inlineLnil_shtml)
         .*. (p_InlineL_Cons .=. inlineLcons_shtml)
         .*. (p_Plain        .=. plain_shtml)
         .*. (p_Bold         .=. bold_shtml) 
         .*. (p_Italics      .=. italics_shtml)
         .*. emptyRecord


--------------------------------
-- Rules for the attribute shtml
--------------------------------


-- Document production
document_shtml = syn shtml $
    do blocks <- at ch_blocks
       return $ blocks # shtml



-- Blocks productions
blockLnil_shtml = syn shtml $ return ""

blockLcons_shtml = syn shtml $
    do block  <- at ch_hd_BlockL_Cons
       blocks <- at ch_tl_BlockL_Cons
       return $ block # shtml ++ blocks # shtml


--blockLcons_shtml = use shtml (nt_BlockL .*. HNil) (++) ""

-- Block productions
header_shtml = syn shtml $
    do level  <- at ch_level_header
       inls   <- at ch_inlines_header
       return $ "<h" ++ show level ++ ">" 
                 ++ inls # shtml
                 ++ "</h" ++ show level ++ ">"

paragraph_shtml = syn shtml $
    do inls <- at ch_inlines_par
       return $ "<p>" 
              ++ inls # shtml
              ++ "</p>"



-- Inline productions

inlineLnil_shtml = syn shtml $ return ""
inlineLcons_shtml = syn shtml $
    do inl  <- at ch_hd_InlineL_Cons
       inls <- at ch_tl_InlineL_Cons
       return $ inl # shtml ++ inls # shtml


plain_shtml = syn shtml $ at ch_str_plainInl

bold_shtml  = syn shtml $ 
                    do inls <- at ch_inlines_boldInl
                       return $ "<b>" 
                              ++ inls # shtml 
                              ++ "</b>"

italics_shtml  = syn shtml $ 
                    do inls <- at ch_inlines_italInl
                       return $ "<i>" 
                              ++ inls # shtml
                              ++ "</i>"




-----------------------
-- building the record

aspDocument     = document_shtml
aspBlockL_Nil   = blockLnil_shtml
aspBlockL_Cons  = blockLcons_shtml
aspHeader       = header_shtml
aspParagraph    = paragraph_shtml
aspInlineL_Nil  = inlineLnil_shtml
aspInlineL_Cons = inlineLcons_shtml
aspPlain        = plain_shtml
aspBold         = bold_shtml
aspItalics      = italics_shtml

semHtml = mkDoc aspBlockL_Cons aspBlockL_Nil aspBold
                aspDocument aspHeader aspInlineL_Cons aspInlineL_Nil
                aspItalics aspParagraph aspPlain
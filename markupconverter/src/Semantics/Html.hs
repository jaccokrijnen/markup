{-# LANGUAGE TemplateHaskell, EmptyDataDecls, NoMonomorphismRestriction #-}
module Semantics.Html where

import Control.Monad

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive

import Decl.Document

$(attLabels ["output"])



-- example doc
doc = Document [Header 1  [Plain "A document"],
                Paragraph [Plain "this is some ", Bold [Plain "really ", Italics [Plain "important"]], Plain " text."],
                Paragraph [Plain "mor text."]]


semHtmlNotExtensible :: Document -> String
semHtmlNotExtensible doc = sem_Document asp_output doc () # output




------------------------------------------
-- Initial aspect output (synthesized html)
------------------------------------------

-- The aspect is a heterogenous list of pairs of production and rule
asp_output =  (p_Document     .=. document_output) 
         .*. (p_BlockL_Nil   .=. blockLnil_output)
         .*. (p_BlockL_Cons  .=. blockLcons_output)
         .*. (p_Header       .=. header_output) 
         .*. (p_Paragraph    .=. paragraph_output)
         .*. (p_InlineL_Nil  .=. inlineLnil_output)
         .*. (p_InlineL_Cons .=. inlineLcons_output)
         .*. (p_Plain        .=. plain_output)
         .*. (p_Bold         .=. bold_output) 
         .*. (p_Italics      .=. italics_output)
         .*. emptyRecord


--------------------------------
-- Rules for the attribute output
--------------------------------


-- Document production
document_output = syn output $
    do blocks <- at ch_blocks
       return $ blocks # output



-- Blocks productions
blockLnil_output = syn output $ return ""

blockLcons_output = syn output $
    do block  <- at ch_hd_BlockL_Cons
       blocks <- at ch_tl_BlockL_Cons
       return $ block # output ++ blocks # output


--blockLcons_output = use output (nt_BlockL .*. HNil) (++) ""

-- Block productions
header_output = syn output $
    do level  <- at ch_level_header
       inls   <- at ch_inlines_header
       return $ "<h" ++ show level ++ ">" 
                 ++ inls # output
                 ++ "</h" ++ show level ++ ">"
                 ++ "\n"

paragraph_output = syn output $
    do inls <- at ch_inlines_par
       return $ "<p>" 
              ++ inls # output
              ++ "</p>"
              ++ "\n"



-- Inline productions

inlineLnil_output = syn output $ return ""
inlineLcons_output = syn output $
    do inl  <- at ch_hd_InlineL_Cons
       inls <- at ch_tl_InlineL_Cons
       return $ inl # output ++ inls # output


plain_output = syn output $ liftM id (at ch_str_plainInl)

bold_output  = syn output $ 
                    do inls <- at ch_inlines_boldInl
                       return $ "<b>" 
                              ++ inls # output 
                              ++ "</b>"

italics_output  = syn output $ 
                    do inls <- at ch_inlines_italInl
                       return $ "<i>" 
                              ++ inls # output
                              ++ "</i>"




-----------------------
-- building the record

aspDocument     = document_output
aspBlockL_Nil   = blockLnil_output
aspBlockL_Cons  = blockLcons_output
aspHeader       = header_output
aspParagraph    = paragraph_output
aspInlineL_Nil  = inlineLnil_output
aspInlineL_Cons = inlineLcons_output
aspPlain        = plain_output
aspBold         = bold_output
aspItalics      = italics_output

semHtml = mkDoc aspBlockL_Cons aspBlockL_Nil aspBold
                aspDocument aspHeader aspInlineL_Cons aspInlineL_Nil
                aspItalics aspParagraph aspPlain
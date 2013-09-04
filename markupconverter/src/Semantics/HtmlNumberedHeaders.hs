{-# LANGUAGE TemplateHaskell, EmptyDataDecls, NoMonomorphismRestriction #-}
module Semantics.HtmlNumberedHeaders where

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive

import Decl.Document
import Semantics.Html
import Semantics.NumberedHeaders


-- overwrite old version of synthesized html
header_output' = synmodM output $ 
    do level  <- at ch_level_header
       inls   <- at ch_inlines_header
       lhs    <- at lhs
       return $ "<h" ++ show level ++ ">" 
                 ++ show (lhs # cHeaderNum) ++ ". "
                 ++ inls # output
                 ++ "</h" ++ show level ++ ">"


asp_output' =  (p_Document     .=. document_output) 
         .*. (p_BlockL_Nil   .=. blockLnil_output)
         .*. (p_BlockL_Cons  .=. blockLcons_output)
         .*. (p_Header       .=. header_output' `ext` header_output) 
         .*. (p_Paragraph    .=. paragraph_output)
         .*. (p_InlineL_Nil  .=. inlineLnil_output)
         .*. (p_InlineL_Cons .=. inlineLcons_output)
         .*. (p_Plain        .=. plain_output)
         .*. (p_Bold         .=. bold_output) 
         .*. (p_Italics      .=. italics_output)
         .*. emptyRecord

semHTML' :: Document -> String
semHTML' doc = sem_Document (asp_cHeaderNum .+. asp_output') doc (cHeaderNum .=. 1 .*. emptyRecord) # output
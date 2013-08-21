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
header_shtml' = synmodM shtml $ 
    do level  <- at ch_level_header
       inls   <- at ch_inlines_header
       lhs    <- at lhs
       return $ "<h" ++ show level ++ ">" 
                 ++ show (lhs # cHeaderNum) ++ ". "
                 ++ inls # shtml
                 ++ "</h" ++ show level ++ ">"


asp_shtml' =  (p_Document     .=. document_shtml) 
         .*. (p_BlockL_Nil   .=. blockLnil_shtml)
         .*. (p_BlockL_Cons  .=. blockLcons_shtml)
         .*. (p_Header       .=. header_shtml' `ext` header_shtml) 
         .*. (p_Paragraph    .=. paragraph_shtml)
         .*. (p_InlineL_Nil  .=. inlineLnil_shtml)
         .*. (p_InlineL_Cons .=. inlineLcons_shtml)
         .*. (p_Plain        .=. plain_shtml)
         .*. (p_Bold         .=. bold_shtml) 
         .*. (p_Italics      .=. italics_shtml)
         .*. emptyRecord

semHTML' :: Document -> String
semHTML' doc = sem_Document (asp_cHeaderNum .+. asp_shtml') doc (cHeaderNum .=. 1 .*. emptyRecord) # shtml
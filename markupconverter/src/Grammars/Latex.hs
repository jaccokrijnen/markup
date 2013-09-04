{-# LANGUAGE Arrows, RecursiveDo, EmptyDataDecls, TemplateHaskell, PostfixOperators, FlexibleContexts #-}

module Grammars.Latex where

import Control.Applicative

import Language.Grammars.Grammar
import Language.Grammars.Murder
import Language.Grammars.Murder.Derive
import Language.Grammars.Murder.UUParsing
import Language.Grammars.AspectAG
import Decl.Document
import Utils

$(csLabels  ["cs_document", "cs_blockL", "cs_paragraph", "cs_header", "cs_inline", "cs_inlineL"])

headers = [(2,"section"), (3, "subsection"), (4, "subsubsection")]

command name pBody = iI ("\\" ++ name ++ "{") pBody "}" Ii

gLatex sem = proc () -> do
    rec
        document     <-addNT-< iI (pDocument sem) blockL Ii

        blockL       <-addNT-< pFoldr (pBlockL_Cons sem, pBlockL_Nil sem) $ 
                                   (iI header Ii) <|> (iI paragraph Ii)
        paragraph    <-addNT-< iI (pParagraph sem) "\\paragraph{" inlineL "}" Ii
        header       <-addNT-< foldr1 (<|>) . flip map headers $ \(x, name) -> 
                                   iI (pHeader sem (sem_Lit x)) "\\" name "{" inlineL "}" Ii

        inline       <-addNT-< iI (pPlain   sem . sem_Lit) "\\plain"  (someExcept "}") "}" Ii
                        <|>    iI (pBold    sem) "\\textbf"  inlineL         "}" Ii
                        <|>    iI (pItalics sem) "\\textit"  inlineL         "}" Ii
        inlineL      <-addNT-< pFoldr (pInlineL_Cons sem, pInlineL_Nil sem) $
                                   iI inline Ii

    exportNTs -<  exportList document ( export cs_document      document
                                      . export cs_blockL        blockL
                                      . export cs_paragraph     paragraph
                                      . export cs_header        header
                                      . export cs_inline        inline
                                      . export cs_inlineL       inlineL)
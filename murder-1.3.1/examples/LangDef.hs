{-# OPTIONS -fcontext-stack=100 #-}

{-# LANGUAGE  Arrows, DoRec #-}

module LangDef where

import qualified Data.Set as Set

import Control.Arrow

import Language.Grammars.Murder
import Language.Grammars.Grammar

import Language.Grammars.Murder.Scanner

import LangSem
import Utils

import Control.Applicative

import Data.HList

import UU.Pretty.Basic

prds = proc () -> do

         rec     root    <- addNT -<  (iI semAGItf exp Ii)
                 exp     <- addNT -<  (iI semLet "let" var  "=" exp "in" exp Ii) 
                                      <|> 
                                      (iI semAdd exp "+" term Ii)
                                      <|> 
                                      iI term Ii 
                 term    <- addNT -<  (iI semMul term  "*"  factor Ii)
                                      <|>
                                      iI factor Ii
                 factor  <- addNT -<  (iI semCst int Ii)
                                      <|> 
                                      (iI semVar var Ii)

         exportNTs -< exportList root  $   export ntExp     exp 
                                       .   export ntTerm    term 
                                       .   export ntFactor  factor  

{-
prds = proc () -> do

         rec     root    <- addNT -<  (iI (`semAGItf` emptyRecord) (ch_expr <=> exp) Ii)
                 exp     <- addNT -<  (iI semLet "let" (ch_lnm   <=> var)  "=" (ch_val <=> exp) "in" (ch_body <=> exp) Ii)
                                      <|>
                                      (iI semAdd (ch_ae1  <=>  exp) "+" (ch_ae2 <=> term) Ii)
                                      <|>
                                      ntPrd term
                 term    <- addNT -<  (iI semMul (ch_me1  <=>  term)  "*"  (ch_me2 <=> factor) Ii)
                                      <|>
                                      ntPrd factor

                 factor  <- addNT -<  (iI semCst (ch_cv <=> int) Ii)
                                      <|> 
                                      (iI semVar (ch_vnm <=> var) Ii)

         exportNTs -< exportList root  $   export ntExp     exp 
                                       .   export ntTerm    term 
                                       .   export ntFactor  factor  
-} 
{-
-- another possible version
prds = proc () -> do

         rec     root    <- addNT -<  (`semAGItf` emptyRecord) <$> ch_expr <=> exp
                 exp     <- addNT -<  semLet <$  tr "let" <*>  ch_lnm   <=> var  <*  tr "=" <*> ch_val <=> exp <* tr "in"  <*>  ch_body <=> exp
                                      <|>
                                      semAdd <$>  ch_ae1  <=>  exp   <* tr "+"  <*>  ch_ae2 <=> term
                                      <|>
                                      ntPrd term
                 term    <- addNT -<  semMul <$>  ch_me1  <=>  term  <* tr "*"  <*>  ch_me2 <=> factor
                                      <|>
                                      ntPrd factor

                 factor  <- addNT -<  semCst <$>  ch_cv <=> int
                                      <|> 
                                      semVar <$>  ch_vnm <=> var

         exportNTs -< exportList root  $   export ntExp     exp 
                                       .   export ntTerm    term 
                                       .   export ntFactor  factor  

-}

{-
gramOpts :: ScanOpts
gramOpts
  =  defaultScanOpts
        {   scoKeywordsTxt      =   Set.fromList ["let", "in"]
        ,   scoSpecChars        =   Set.fromList "=+*"
        ,   scoDollarIdent      =   True
        }
-} 


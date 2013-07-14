{-# OPTIONS -fcontext-stack=100 #-}

{-# LANGUAGE  Arrows, DoRec #-}

module Language.Oberon0.L3.Gram (module Language.Oberon0.L3.Gram, module Language.Oberon0.L2.Gram) where

import Language.Oberon0.L2.Gram
import Language.Oberon0.L3.Decl

import Control.Arrow
import Control.Applicative
import Data.Set (union,fromList)

import Language.Grammars.Murder
import Language.Grammars.Grammar

import Language.Grammars.AspectAG

fpVar  sf = pParam sf (sem_Lit VarP)
fpVal  sf = pParam sf (sem_Lit ValP)

l3 sf = proc imported -> do
         let decls      = getNT cs_Declarations       imported 
         let statement  = getNT cs_Statement          imported 
         let ss         = getNT cs_StatementSequence  imported 
         let exp        = getNT cs_Expression         imported 
         let ident      = getNT cs_Ident              imported 
         let idl        = getNT cs_IdentL             imported 
         let typ        = getNT cs_Type               imported 


         rec  addProds   -<  (statement,     iI (pProcCStmt  sf) ident params Ii)

              params     <- addNT -<  iI  "(" paraml ")" Ii
                                  <|> iI (pExpressionL_Nil sf) Ii

              paraml     <- addNT -<  iI (pExpressionL_Cons sf) exp (pFoldr (pExpressionL_Cons sf, pExpressionL_Nil sf) (iI "," exp Ii)) Ii
                                  <|> iI (pExpressionL_Nil sf) Ii

              updProds   -<  (decls,  \declarations -> iI (pExtDeclarations sf) declarations procDeclL Ii)

              procDeclL  <- addNT -<  pFoldr (pDeclL_Cons' sf, pDeclL_Nil' sf) (iI procDecl Ii)

              procDecl   <- addNT -< iI (pProcDecl  sf)   "PROCEDURE" ident fparams ";"
                                                          decls 
                                                          (pMaybe (pEmptyStmt' sf, id) (iI "BEGIN" ss Ii)) 
                                                          "END" ident ";" Ii

              fparams    <- addNT -<  iI  "(" fparaml ")" Ii
                                  <|> iI (pParamL_Nil sf) Ii

              fparaml    <- addNT -<  iI (pParamL_Cons sf) fparam (pFoldr (pParamL_Cons sf, pParamL_Nil sf) (iI ";" fparam Ii)) Ii
                                  <|> iI (pParamL_Nil sf) Ii

              fparam     <- addNT -<  iI  (fpVar sf) "VAR" idl ":" typ Ii <|> iI (fpVal sf)  idl ":" typ Ii 

         exportNTs -< imported

l3Kws = union l2Kws $ fromList ["PROCEDURE"]


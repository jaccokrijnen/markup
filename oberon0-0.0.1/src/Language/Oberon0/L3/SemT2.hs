{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction, ScopedTypeVariables  #-}

module Language.Oberon0.L3.SemT2 (module Language.Oberon0.L3.SemT2, module Language.Oberon0.L2.SemT2) where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Language.Grammars.Grammar (DTerm(..),Pos(..))

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import qualified Data.Map as Map

import Language.Oberon0.L1.SemT2 (aspEmptyStmt, aspDeclL_Cons, aspDeclL_Nil)


import Language.Oberon0.L3.Decl

import qualified Language.Oberon0.L3.SemT1 as T1


import qualified Language.Oberon0.L2.SemT2 as L2
import Language.Oberon0.L2.SemT2 (l1t2,l2t2,spp,sppl,sidl,senv,ienv,serr,spos,SymbolInfo(..),NameDef(..),NInfo,checkName)


---- T2 (L3) Name binding


-- senv

senvNT = nt_Param  .*. nt_ParamL .*. L2.senvNT

senvRule (_ :: a) = use senv senvNT Map.union (Map.empty :: Map.Map String (NInfo a)) 


senvProcDecl r  
          = syn senv $ do  id  <- at ch_id_ProcDecl
                           return $ Map.singleton  (value id) 
                                                   (SI (NameDef (pos id) "Prc" "procedure") r)


-- ienv

ienvNT = nt_Param  .*. nt_ParamL .*. nt_ExpressionL .*. L2.ienvNT

ienvRule _ = copy ienv ienvNT


stdProcs r = Map.fromList  [ ("Read",    SI (NameDef (Pos (-1) (-1)) "Prc" "standard procedure") r)
                           , ("Write",   SI (NameDef (Pos (-1) (-1)) "Prc" "standard procedure") r)
                           , ("WriteLn", SI (NameDef (Pos (-1) (-1)) "Prc" "standard procedure") r) ]

ienvIni r = Map.union (L2.ienvIni r) (stdProcs r)

ienvExtDeclarations r      
          = inh ienv ienvNT $ do  
                          lhs     <- at lhs
                          decls   <- at ch_decls_ExtDeclarations
                          prcdecl <- at ch_prcdecl_ExtDeclarations

                          let  local = Map.union (Map.filter (\(SI (NameDef _ k _) _) -> k /= "Var") (decls # senv)) (prcdecl # senv)
                          let  env   = Map.union local (lhs # ienv)
                          return $ ch_decls_ExtDeclarations   .=. Map.union (decls # senv) (lhs # ienv) .*.
                                   ch_prcdecl_ExtDeclarations .=. env .*.
                                   emptyRecord 

ienvProcDecl  _  
          = inh ienv ienvNT $ do  
                          lhs     <- at lhs
                          params  <- at ch_params_ProcDecl
                          decls   <- at ch_decls_ProcDecl
                          stmts   <- at ch_stmts_ProcDecl

                          let  local = Map.union (params # senv) (decls # senv)
                               env   = Map.union local (lhs # ienv)
                          return $ ch_params_ProcDecl .=. Map.union (params # senv) (lhs # ienv) .*.
                                   ch_stmts_ProcDecl  .=. env .*.
                                   ch_decls_ProcDecl  .=. (lhs # ienv) .*.
                                   emptyRecord 


-- serr

serrNT = nt_Param  .*. nt_ParamL .*. nt_ExpressionL .*. L2.serrNT


serrRule = use serr serrNT (++) ([] :: [String]) 


serrProcCStmt
          = syn serr $ do id     <- at ch_id_ProcCStmt
                          params <- at ch_params_ProcCStmt
                          lhs    <- at lhs
                          return $  checkName id (lhs # ienv) ["Prc"] "a procedure call" ++ params # serr 



---- Aspects

aspExtDeclarations     r = (senvRule r) `ext` (ienvExtDeclarations r) `ext` serrRule `ext` T1.aspExtDeclarations
aspProcDecl            r = (senvProcDecl r) `ext` (ienvProcDecl r) `ext` serrRule `ext` T1.aspProcDecl
aspParamL_Cons         r = (senvRule r) `ext` (ienvRule r) `ext` serrRule `ext` T1.aspParamL_Cons
aspParamL_Nil          r = (senvRule r) `ext` (ienvRule r) `ext` serrRule `ext` T1.aspParamL_Nil
aspParam               r = (senvRule r) `ext` (ienvRule r) `ext` serrRule `ext` T1.aspParam
aspProcCStmt           r = (ienvRule r) `ext` serrProcCStmt `ext` T1.aspProcCStmt
aspExpressionL_Cons    r = (ienvRule r) `ext` serrRule `ext` T1.aspExpressionL_Cons
aspExpressionL_Nil     r = (ienvRule r) `ext` serrRule `ext` T1.aspExpressionL_Nil


---- Semantic Functions

l3t2 = mkL3  (aspExtDeclarations ()) (aspProcDecl ()) (aspParamL_Cons ()) (aspParamL_Nil ()) (aspParam ())     
             (aspProcCStmt ()) (aspExpressionL_Cons ()) (aspExpressionL_Nil ())
             (aspEmptyStmt ()) (aspDeclL_Cons ()) (aspDeclL_Nil ())


{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE TemplateHaskell, EmptyDataDecls, NoMonomorphismRestriction #-}

module Language.Oberon0.L3.SemT1 (module Language.Oberon0.L3.SemT1, module Language.Oberon0.L2.SemT1) where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Language.Grammars.Grammar (DTerm(..), Pos(..))

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import UU.Pretty

import Language.Oberon0.L3.Decl

import Language.Oberon0.L1.SemT1 (aspEmptyStmt, aspDeclL_Cons, aspDeclL_Nil)

import qualified Language.Oberon0.L2.SemT1 as L2
import Language.Oberon0.L2.SemT1 (l1t1,l2t1,spp,sppl,sidl)





---- T1(L3) Pretty-Printing


sppNT =  nt_Param .*. L2.sppNT

sppExtDeclarations
          = syn spp $ do
                          decls    <- at ch_decls_ExtDeclarations
                          prcdecl  <- at ch_prcdecl_ExtDeclarations
                          return $  decls # spp >-< 
                                    vlist (prcdecl # sppl)



sppParam 
          = syn spp $ do
                          kind    <- at ch_kind_Param
                          idl     <- at ch_idl_Param
                          typ     <- at ch_typ_Param

                          return $ case (map pp (idl # sidl)) of
                                    []    -> empty
                                    ppidl -> show kind >#<  pp_block "" "" ", " ppidl >#< ":" >#< typ # spp 

sppProcDecl 
          = syn spp $ do
                          id      <- at ch_id_ProcDecl
                          params  <- at ch_params_ProcDecl
                          decls   <- at ch_decls_ProcDecl
                          stmts   <- at ch_stmts_ProcDecl
                          idend   <- at ch_idend_ProcDecl

                          return $ "PROCEDURE" >#< value id >#< pp_block "(" ")" ";" (params # sppl) >#< ";" >-< 
                                   decls # spp >-< pp_block " BEGIN " (" END " ++ value idend ++ ";") ";" (stmts # sppl)


-- sppl

spplNT = nt_ExpressionL .*. nt_ParamL .*. L2.spplNT

spplProcCStmt
          = syn sppl $ do  
                          id     <- at ch_id_ProcCStmt
                          params <- at ch_params_ProcCStmt
                          return $ [ value id >#<  pp_block "(" ")" "," (params # sppl) ]


spplParamL_Cons    
          = syn sppl $ do  hd  <- at ch_hd_ParamL_Cons
		    	   tl  <- at ch_tl_ParamL_Cons
                           return $ (hd # spp) : (tl # sppl)

spplParamL_Nil    
          = syn sppl $ do  return $ []


spplExpressionL_Cons    
          = syn sppl $ do  h <- at ch_hd_ExpressionL_Cons
                           t <- at ch_tl_ExpressionL_Cons
                           return $ (h # spp) : (t # sppl)
spplExpressionL_Nil    
          = syn sppl $ do  return $ ([] :: [PP_Doc]) 


---- Aspects

aspExtDeclarations          = sppExtDeclarations

aspProcDecl                 = sppProcDecl

aspParamL_Cons              = spplParamL_Cons 
aspParamL_Nil               = spplParamL_Nil 

aspParam                    = sppParam

aspProcCStmt                = spplProcCStmt

aspExpressionL_Cons         = spplExpressionL_Cons
aspExpressionL_Nil          = spplExpressionL_Nil

---- Semantic Functions



l3t1 = mkL3  aspExtDeclarations aspProcDecl aspParamL_Cons aspParamL_Nil aspParam     
             aspProcCStmt aspExpressionL_Cons aspExpressionL_Nil
             aspEmptyStmt aspDeclL_Cons aspDeclL_Nil
 


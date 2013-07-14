{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Language.Oberon0.L4.SemT2 (module Language.Oberon0.L4.SemT2, module Language.Oberon0.L3.SemT2) where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Language.Grammars.Grammar (DTerm(..),Pos(..))

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import qualified Data.Map as Map


import Language.Oberon0.L4.Decl

import qualified Language.Oberon0.L4.SemT1 as T1


import qualified Language.Oberon0.L3.SemT2 as L3
import Language.Oberon0.L3.SemT2 (l1t2,l2t2,l3t2,spp,sppl,sidl,senv,ienv,serr,spos,SymbolInfo(..),NameDef(..),NInfo,checkName)


---- T2 (L4) Name binding


-- spos

sposRecordType = syn spos $ do  return $ Pos 0 0 --TODO?


sposArrayType  = syn spos $ do  return $ Pos 0 0 --TODO?



-- senv

senvNT = nt_Field  .*. nt_FieldL .*. L3.senvNT

senvRule (_ :: a) = use senv senvNT Map.union (Map.empty :: Map.Map String (NInfo a)) 



-- ienv

ienvNT = nt_Select  .*. nt_SelectL .*. nt_Field  .*. nt_FieldL .*. L3.ienvNT

ienvRule _ = copy ienv ienvNT

ienvIni  = L3.ienvIni

ienvRecordType r = inh ienv ienvNT $ do lhs    <- at lhs
                                        fields <- at ch_fields_RecordType

                                        return $ ch_fields_RecordType  .=. Map.union (fields # senv) (lhs # ienv) .*.
                                                 emptyRecord 

-- serr

serrNT = nt_Select  .*. nt_SelectL .*. nt_Field  .*. nt_FieldL .*. L3.serrNT


serrRule = use serr serrNT (++) ([] :: [String]) 

serrSelExp       = syn serr $ do id   <- at ch_id_SelExp
                                 lhs <- at lhs
                                 return $  checkName id (lhs # ienv) ["Var"] "an expression"


serrAssigSelStmt = syn serr $ do id   <- at ch_id_AssigSelStmt 
                                 exp  <- at ch_exp_AssigSelStmt
                                 lhs  <- at lhs 
                                 return $  checkName id (lhs # ienv) ["Var"] "an assignment" ++ exp # serr 




---- Aspects

aspArrayType                r = sposArrayType  `ext` (ienvRule r)       `ext` serrRule `ext`  T1.aspArrayType

aspRecordType               r = sposRecordType `ext` (ienvRecordType r) `ext` serrRule `ext`  T1.aspRecordType

aspFieldL_Cons              r = (senvRule r) `ext` (ienvRule r) `ext` serrRule `ext`  T1.aspFieldL_Cons 
aspFieldL_Nil               r = (senvRule r) `ext` (ienvRule r) `ext` serrRule `ext`  T1.aspFieldL_Nil 

aspField                    r = (senvRule r) `ext` (ienvRule r) `ext` serrRule `ext`  T1.aspField
aspEmptyField               r = (senvRule r) `ext` (ienvRule  r) `ext` serrRule `ext`  T1.aspEmptyField

aspSelExp                   r = (ienvRule r) `ext` serrSelExp `ext`  T1.aspSelExp

aspSelectL_Cons             r = (ienvRule r) `ext` serrRule `ext`  T1.aspSelectL_Cons
aspSelectL_Nil              r = (ienvRule r) `ext` serrRule `ext`  T1.aspSelectL_Nil

aspSelField                 r = (ienvRule r) `ext` serrRule `ext`  T1.aspSelField
aspSelArray                 r = (ienvRule r) `ext` serrRule `ext`  T1.aspSelArray


aspAssigSelStmt             r = (ienvRule r) `ext` serrAssigSelStmt `ext`  T1.aspAssigSelStmt

---- Semantic Functions

l4t2 = mkL4  (aspArrayType ()) (aspAssigSelStmt ()) (aspEmptyField ()) (aspField ()) (aspFieldL_Cons ()) (aspFieldL_Nil ())
             (aspRecordType ()) (aspSelArray ()) (aspSelExp ()) (aspSelField ()) (aspSelectL_Cons ()) (aspSelectL_Nil ())


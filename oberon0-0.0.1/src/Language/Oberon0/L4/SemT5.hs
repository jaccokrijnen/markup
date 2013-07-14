{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction, TemplateHaskell #-}

module Language.Oberon0.L4.SemT5 (module Language.Oberon0.L4.SemT5, module Language.Oberon0.L3.SemT5) where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Language.Grammars.Grammar (DTerm(..),Pos(..))

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import Language.C


import Language.Oberon0.L4.Decl

import qualified Language.Oberon0.L4.SemT3 as T3

import qualified Language.Oberon0.L1.SemT5 as L1

import qualified Language.Oberon0.L3.SemT5 as L3
import Language.Oberon0.L3.SemT5 (l2t5
                ,spp,sidl,senv,ienv,serr,spos,sty,sterr,SymbolInfo(..),NameDef(..),TInfo(..),baseType,intType,boolType,unkType,findType
                ,scgen,scdec,scdecl,scty,scexp,scstmt,ipre,mapIdent,updMap,compStmt)


---- T5 (L4) Code Generation

-- ipre

ipreNT =   nt_Select .*. nt_SelectL .*. nt_Field  .*. nt_FieldL .*. L3.ipreNT 

ipreRule = copy ipre ipreNT


-- senv

senvNT = T3.senvNT

-- ienv

ienvNT =  T3.ienvNT

ienvIni = L3.ienvIni


ienvRule _ = copy ienv ienvNT

-- scsel

$(attLabels ["scsel"])

scselNT =   nt_Select .*. nt_SelectL .*. hNil

scselSelField  = syn scsel $ do  ids  <- at ch_id_SelField
                                 return $ \exp -> CMember exp (internalIdent $ value ids) False undefNode 


scselSelArray  = syn scsel $ do  ind  <- at ch_exp_SelArray
                                 return $ \exp -> CIndex exp (ind # scexp) undefNode 


scselSelectL_Cons = syn scsel $ do  h <- at ch_hd_SelectL_Cons
                                    t <- at ch_tl_SelectL_Cons
                                    return $ (t # scsel) . (h # scsel)
scselSelectL_Nil  = syn scsel $ do  return $ id

-- scexp

scexpNT = L3.scexpNT

scexpSelExp = syn scexp $ do  lhs  <- at lhs
                              id   <- at ch_id_SelExp
                              sel  <- at ch_sel_SelExp
                              return $ (sel # scsel) (CVar (mapIdent (value id) (lhs # ienv)) undefNode)


-- scstmt

scstmtNT = L3.scstmtNT

scstmtAssigSelStmt = syn scstmt $ do  lhs  <- at lhs
                                      id   <- at ch_id_AssigSelStmt
                                      sel  <- at ch_sel_AssigSelStmt
                                      exp  <- at ch_exp_AssigSelStmt
                                      let rhs = (sel # scsel) (CVar (mapIdent (value id) (lhs # ienv)) undefNode)
 
                                      return $ CExpr (Just $ CAssign CAssignOp rhs (exp # scexp) undefNode) undefNode

-- scdec

scdecNT = nt_Field .*. nt_FieldL .*. L3.scdecNT

scdecRule = use scdec scdecNT (++) ([] :: [CDeclaration NodeInfo]) 


scdecField = syn scdec $ do idl  <- at ch_idl_Field
                            typ  <- at ch_typ_Field
                            let (tys,tyd) = typ # scty
                            return $ map (\name -> ( CDecl  [ tys ] 
                                                            [(Just (CDeclr (Just $ internalIdent name) tyd Nothing [] undefNode),Nothing,Nothing)] 
                                                            undefNode)) 
                                         (idl # sidl)


-- scty

sctyArrayType  = syn scty $ do exp  <- at ch_exp_ArrayType
                               typ  <- at ch_typ_ArrayType
                               let (tys,tyd) = typ # scty
                               return $ (tys, tyd ++ [CArrDeclr [] (CArrSize False (exp # scexp)) undefNode] ) 

sctyRecordType = syn scty $ do fields  <- at ch_fields_RecordType
                               return $ (CTypeSpec (CSUType (CStruct CStructTag Nothing (Just (fields # scdec)) [] undefNode) undefNode) , [])




---- Aspects
pMap :: a -> SymbolInfo String a
pMap _ = undefined

aspArrayType                r = ipreRule `ext` (ienvRule r) `ext` sctyArrayType  `ext` (T3.aspArrayType (pMap r))

aspRecordType               r = ipreRule `ext` (ienvRule r) `ext` sctyRecordType `ext` (T3.aspRecordType (pMap r))

aspFieldL_Cons              r = ipreRule `ext` (ienvRule r) `ext` scdecRule  `ext`  (T3.aspFieldL_Cons (pMap r)) 
aspFieldL_Nil               r = ipreRule `ext` (ienvRule r) `ext` scdecRule  `ext`  (T3.aspFieldL_Nil (pMap r)) 

aspField                    r = ipreRule `ext` (ienvRule r) `ext` scdecField `ext`  (T3.aspField (pMap r))
aspEmptyField               r = ipreRule `ext` (ienvRule r) `ext` scdecRule  `ext`  (T3.aspEmptyField (pMap r))

aspSelExp                   r = ipreRule `ext` (ienvRule r) `ext` scexpSelExp `ext` (T3.aspSelExp (pMap r))

aspSelectL_Cons             r = ipreRule `ext` (ienvRule r) `ext` scselSelectL_Cons `ext` (T3.aspSelectL_Cons (pMap r))
aspSelectL_Nil              r = ipreRule `ext` (ienvRule r) `ext` scselSelectL_Nil  `ext` (T3.aspSelectL_Nil (pMap r))

aspSelField                 r = ipreRule `ext` (ienvRule r) `ext` scselSelField `ext` (T3.aspSelField (pMap r))
aspSelArray                 r = ipreRule `ext` (ienvRule r) `ext` scselSelArray `ext` (T3.aspSelArray (pMap r))


aspAssigSelStmt             r = ipreRule `ext` (ienvRule r) `ext` scstmtAssigSelStmt `ext`  (T3.aspAssigSelStmt (pMap r))


---- Semantic Functions
mkL1''' = T3.mkL1'''
mkL3' = T3.mkL3'

l1t5 = mkL1''' (L1.aspAssigStmt ()) (L1.aspBoolBOpExp ()) (L1.aspBoolExp ()) (L1.aspBoolUOpExp ()) (L1.aspCondStmt ()) (L1.aspCondStmtL_Cons ()) 
               (L1.aspCondStmtL_Nil ()) (L1.aspCstDecl ()) (L1.aspDeclL_Cons ()) (L1.aspDeclL_Nil ()) (L1.aspDecls ()) (L1.aspEmptyStmt ()) 
               (L1.aspIdExp ()) (L1.aspIdentL_Cons ()) (L1.aspIdentL_Nil ()) (L1.aspIfStmt ()) (L1.aspIntBOpExp ()) (L1.aspIntCmpExp ()) (L1.aspIntExp ()) 
               (L1.aspIntUOpExp ()) (L1.aspMaybeElseStmt_Just ()) (L1.aspMaybeElseStmt_Nothing ()) (L1.aspModule ()) (L1.aspParExp ()) 
               (L1.aspSeqStmt ()) (L1.aspTypDecl ()) (L1.aspType ()) (L1.aspVarDecl ()) (L1.aspWhileStmt ()) 

l3t5 = mkL3' (L3.aspExtDeclarations ()) (L3.aspProcDecl ()) (L3.aspParamL_Cons ()) (L3.aspParamL_Nil ()) (L3.aspParam ())     
             (L3.aspProcCStmt ()) (L3.aspExpressionL_Cons ()) (L3.aspExpressionL_Nil ())
             (L1.aspEmptyStmt ()) (L1.aspDeclL_Cons ()) (L1.aspDeclL_Nil ())

mkL4' = T3.mkL4'

l4t5 = mkL4' (aspArrayType ()) (aspAssigSelStmt ()) (aspEmptyField ()) (aspField ()) (aspFieldL_Cons ()) (aspFieldL_Nil ())
             (aspRecordType ()) (aspSelArray ()) (aspSelExp ()) (aspSelField ()) (aspSelectL_Cons ()) (aspSelectL_Nil ())


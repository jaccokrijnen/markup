{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE TemplateHaskell, EmptyDataDecls, NoMonomorphismRestriction #-}

module Language.Oberon0.L2.SemT2 (module Language.Oberon0.L2.SemT2, module Language.Oberon0.L1.SemT2) where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Language.Grammars.Grammar (DTerm(..),Pos)

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import qualified Data.Map as Map

import Language.Oberon0.L2.Decl

import qualified Language.Oberon0.L2.SemT1 as T1

import qualified Language.Oberon0.L1.SemT2 as L1
import Language.Oberon0.L1.SemT2 (spp,sppl,sidl,senv,ienv,serr,spos,SymbolInfo(..),NameDef(..),NInfo,checkName)


---- T2 (L2) Name binding

-- senv

senvNT = L1.senvNT

-- ienv

ienvNT = nt_CaseL .*. nt_Case  .*. nt_LabelL .*. nt_Label .*. L1.ienvNT

ienvIni = L1.ienvIni

fenv :: r -> (Map.Map String (NInfo r) ->  Map.Map String (NInfo r))
          -> (Map.Map String (NInfo r) ->  Map.Map String (NInfo r))
fenv r = id

ienvForStmt r = inhupdM ienv ienvNT $ do id <- at ch_id_ForStmt
                                         let fid = value id
                                         return $ ch_start_ForStmt .=. fenv r (\env -> Map.delete fid env) .*.
                                                  ch_stop_ForStmt  .=. fenv r (\env -> Map.delete fid env) .*.
                                                  ch_step_ForStmt  .=. fenv r (\env -> Map.delete fid env) .*.
                                                  ch_ss_ForStmt    .=. fenv r (\env -> env) .*.
                                                  emptyRecord 


-- serr

serrNT = nt_CaseL .*. nt_Case  .*. nt_Label .*. L1.serrNT


serrRule = use serr serrNT (++) ([] :: [String]) 


serrForStmt = synmodM serr $ do id    <- at ch_id_ForStmt
                                start <- at ch_start_ForStmt
                                stop  <- at ch_stop_ForStmt
                                step  <- at ch_step_ForStmt
                                ss    <- at ch_ss_ForStmt
                                lhs   <- at lhs                                 
                                return $  checkName id (lhs # ienv) ["Var"] "a FOR statement" ++
                                          start # serr ++ stop # serr ++ step # serr ++ ss # serr




---- Aspects

aspForStmt     r _macroForStmt     = (ienvForStmt r) `ext` serrForStmt `ext` (T1.aspForStmt _macroForStmt)
aspTo          _                   = T1.aspTo
aspDownto      _                   = T1.aspDownto

aspCaseStmt    _ _macroCaseStmt   = T1.aspCaseStmt   _macroCaseStmt   
aspCaseL_Cons  _ _macroCaseL_Cons = T1.aspCaseL_Cons _macroCaseL_Cons 
aspCaseL_Nil   _ _macroCaseL_Nil  = T1.aspCaseL_Nil  _macroCaseL_Nil 
aspCase        _ _macroCase      = T1.aspCase       _macroCase 

aspLabelL_Cons _ _macroLabelL_Cons = T1.aspLabelL_Cons _macroLabelL_Cons 
aspLabelL_Nil  _ _macroLabelL_Nil  = T1.aspLabelL_Nil  _macroLabelL_Nil 

aspExpreLbl    _ _macroExpreLbl   = T1.aspExpreLbl   _macroExpreLbl

aspRangeLbl    _ _macroRangeLbl   = T1.aspRangeLbl   _macroRangeLbl



aspCst1Exp     _ _macroCst1Exp    = T1.aspCst1Exp _macroCst1Exp

---- Semantic Functions

l2t2 = mkL2'   (aspCase ()) (aspCaseL_Cons ()) (aspCaseL_Nil ())  
               (aspCaseStmt ()) (aspCst1Exp ()) (aspDownto ()) (aspExpreLbl ())
               (aspForStmt ()) (aspLabelL_Cons ()) (aspLabelL_Nil ()) (aspRangeLbl ()) (aspTo ()) 
       
               (L1.aspAssigStmt ()) (L1.aspBoolBOpExp ()) (L1.aspBoolExp ()) (L1.aspCondStmt ()) (L1.aspCondStmtL_Cons ()) (L1.aspCondStmtL_Nil ())
               (L1.aspIfStmt ()) (L1.aspIdExp ()) (L1.aspIntBOpExp ()) (L1.aspIntCmpExp ()) (L1.aspIntExp ()) (L1.aspSeqStmt ()) (L1.aspWhileStmt ())

mkL1' = T1.mkL1'

l1t2 = mkL1'  (L1.aspAssigStmt ()) (L1.aspBoolBOpExp ()) (L1.aspBoolExp ()) (L1.aspBoolUOpExp ()) (L1.aspCondStmt ()) (L1.aspCondStmtL_Cons ()) 
              (L1.aspCondStmtL_Nil ()) (L1.aspCstDecl ()) (L1.aspDeclL_Cons ()) (L1.aspDeclL_Nil ()) (L1.aspDecls ()) (L1.aspEmptyStmt ()) 
              (L1.aspIdExp ()) (L1.aspIdentL_Cons ()) (L1.aspIdentL_Nil ()) (L1.aspIfStmt ()) (L1.aspIntBOpExp ()) (L1.aspIntCmpExp ()) (L1.aspIntExp ()) 
              (L1.aspIntUOpExp ()) (L1.aspMaybeElseStmt_Just ()) (L1.aspMaybeElseStmt_Nothing ()) (L1.aspModule ()) (L1.aspParExp ()) 
              (L1.aspSeqStmt ()) (L1.aspTypDecl ()) (L1.aspType ()) (L1.aspVarDecl ()) (L1.aspWhileStmt ()) 



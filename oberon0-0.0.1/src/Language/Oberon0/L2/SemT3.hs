{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction #-}

module Language.Oberon0.L2.SemT3 (module Language.Oberon0.L2.SemT3, module Language.Oberon0.L1.SemT3) where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Language.Grammars.Grammar (DTerm(..),Pos(..))

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import Data.List


import Language.Oberon0.L2.Decl
import qualified Language.Oberon0.L2.SemT2 as T2


import qualified Language.Oberon0.L1.SemT3 as L1
import Language.Oberon0.L1.SemT3 (spp,sidl,sval,senv,ienv,serr,spos,sty,sterr,check,SymbolInfo(..),NameDef(..),TInfo(..),baseType,intType,boolType,unkType,findType)


---- T3 (L2) Type-checking

styNT   = L1.styNT
svalNT  = L1.svalNT
senvNT  = L1.senvNT
ienvNT  = L1.ienvNT
ienvIni = L1.ienvIni

-- sterr

sterrNT = nt_CaseL .*. nt_Case .*. nt_Label .*. L1.sterrNT 

sterrRule = use sterr sterrNT (++) ([] :: [String]) 


sterrForStmt = synmodM sterr $ do id    <- at ch_id_ForStmt
                                  start <- at ch_start_ForStmt
                                  stop  <- at ch_stop_ForStmt
                                  step  <- at ch_step_ForStmt
                                  ss    <- at ch_ss_ForStmt
                                  lhs   <- at lhs
                                  return $ check (pos id) intType (findType (value id) (lhs # ienv)) ++
                                           check (start # spos) intType (start # sty) ++
                                           check (stop # spos)  intType (stop # sty) ++
                                           check (step # spos)  intType (step # sty) ++
                                           start # sterr ++ stop # sterr ++ step # sterr ++ ss # sterr


 


---- Aspects
pTInfo :: a -> SymbolInfo TInfo a
pTInfo _ = undefined

aspForStmt     r _macroForStmt    = sterrForStmt `ext` (T2.aspForStmt (pTInfo r) _macroForStmt)
aspTo          r                 = T2.aspTo     (pTInfo r)
aspDownto      r                 = T2.aspDownto (pTInfo r)

aspCaseStmt    r _macroCaseStmt   = T2.aspCaseStmt   (pTInfo r) _macroCaseStmt   
aspCaseL_Cons  r _macroCaseL_Cons = T2.aspCaseL_Cons (pTInfo r) _macroCaseL_Cons 
aspCaseL_Nil   r _macroCaseL_Nil  = T2.aspCaseL_Nil  (pTInfo r) _macroCaseL_Nil 
aspCase        r _macroCase       = T2.aspCase       (pTInfo r) _macroCase 

aspLabelL_Cons  r _macroLabelL_Cons = T2.aspLabelL_Cons (pTInfo r) _macroLabelL_Cons 
aspLabelL_Nil   r _macroLabelL_Nil  = T2.aspLabelL_Nil  (pTInfo r) _macroLabelL_Nil 

aspExpreLbl    r _macroExpreLbl   = T2.aspExpreLbl  (pTInfo r) _macroExpreLbl
aspRangeLbl    r _macroRangeLbl   = T2.aspRangeLbl  (pTInfo r) _macroRangeLbl



aspCst1Exp     r _macroCst1Exp    = T2.aspCst1Exp (pTInfo r) _macroCst1Exp

---- Semantic Functions

l2t3 = mkL2'   (aspCase ()) (aspCaseL_Cons ()) (aspCaseL_Nil ())  
               (aspCaseStmt ()) (aspCst1Exp ()) (aspDownto ()) (aspExpreLbl ())
               (aspForStmt ()) (aspLabelL_Cons ()) (aspLabelL_Nil ()) (aspRangeLbl ()) (aspTo ()) 
       
               (L1.aspAssigStmt ()) (L1.aspBoolBOpExp ()) (L1.aspBoolExp ()) (L1.aspCondStmt ()) (L1.aspCondStmtL_Cons ()) (L1.aspCondStmtL_Nil ())
               (L1.aspIfStmt ()) (L1.aspIdExp ()) (L1.aspIntBOpExp ()) (L1.aspIntCmpExp ()) (L1.aspIntExp ()) (L1.aspSeqStmt ()) (L1.aspWhileStmt ())

mkL1' = T2.mkL1'

l1t3 = mkL1'  (L1.aspAssigStmt ()) (L1.aspBoolBOpExp ()) (L1.aspBoolExp ()) (L1.aspBoolUOpExp ()) (L1.aspCondStmt ()) (L1.aspCondStmtL_Cons ()) 
              (L1.aspCondStmtL_Nil ()) (L1.aspCstDecl ()) (L1.aspDeclL_Cons ()) (L1.aspDeclL_Nil ()) (L1.aspDecls ()) (L1.aspEmptyStmt ()) 
              (L1.aspIdExp ()) (L1.aspIdentL_Cons ()) (L1.aspIdentL_Nil ()) (L1.aspIfStmt ()) (L1.aspIntBOpExp ()) (L1.aspIntCmpExp ()) (L1.aspIntExp ()) 
              (L1.aspIntUOpExp ()) (L1.aspMaybeElseStmt_Just ()) (L1.aspMaybeElseStmt_Nothing ()) (L1.aspModule ()) (L1.aspParExp ()) 
              (L1.aspSeqStmt ()) (L1.aspTypDecl ()) (L1.aspType ()) (L1.aspVarDecl ()) (L1.aspWhileStmt ()) 


{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction #-}

module Language.Oberon0.L2.SemT5 (module Language.Oberon0.L2.SemT5, module Language.Oberon0.L1.SemT5) where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Language.Grammars.Grammar (DTerm(..),Pos(..))

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import Data.List


import Language.Oberon0.L2.Decl
import qualified Language.Oberon0.L2.SemT3 as T3


import qualified Language.Oberon0.L1.SemT5 as L1
import Language.Oberon0.L1.SemT5 (spp,sidl,senv,ienv,serr,spos,sty,sterr,check,SymbolInfo(..),NameDef(..),TInfo(..),baseType,intType,boolType,unkType,findType
                ,scgen,scdecl,scty,scexp,scstmt,ipre,mapIdent,updMap,compStmt)


---- T5 (L2) Code Generation

ipreNT  = L1.ipreNT
senvNT  = L1.senvNT
ienvNT  = L1.ienvNT
ienvIni = L1.ienvIni


scdeclNT = L1.scdeclNT
scexpNT  = L1.scexpNT
scstmtNT = L1.scstmtNT

---- Aspects
pMap :: a -> SymbolInfo String a
pMap _ = undefined

aspForStmt     r _macroForStmt    = T3.aspForStmt (pMap r) _macroForStmt
aspTo          r                  = T3.aspTo     (pMap r)
aspDownto      r                  = T3.aspDownto (pMap r)

aspCaseStmt    r _macroCaseStmt   = T3.aspCaseStmt   (pMap r) _macroCaseStmt   
aspCaseL_Cons  r _macroCaseL_Cons = T3.aspCaseL_Cons (pMap r) _macroCaseL_Cons 
aspCaseL_Nil   r _macroCaseL_Nil  = T3.aspCaseL_Nil  (pMap r) _macroCaseL_Nil 
aspCase        r _macroCase       = T3.aspCase       (pMap r) _macroCase 

aspLabelL_Cons  r _macroLabelL_Cons = T3.aspLabelL_Cons (pMap r) _macroLabelL_Cons 
aspLabelL_Nil   r _macroLabelL_Nil  = T3.aspLabelL_Nil  (pMap r) _macroLabelL_Nil 

aspExpreLbl    r _macroExpreLbl   = T3.aspExpreLbl   (pMap r) _macroExpreLbl
aspRangeLbl    r _macroRangeLbl   = T3.aspRangeLbl   (pMap r) _macroRangeLbl



aspCst1Exp     r _macroCst1Exp    = T3.aspCst1Exp (pMap r) _macroCst1Exp

---- Semantic Functions

l2t5 = mkL2'   (aspCase ()) (aspCaseL_Cons ()) (aspCaseL_Nil ())  
               (aspCaseStmt ()) (aspCst1Exp ()) (aspDownto ()) (aspExpreLbl ())
               (aspForStmt ()) (aspLabelL_Cons ()) (aspLabelL_Nil ()) (aspRangeLbl ()) (aspTo ()) 
       
               (L1.aspAssigStmt ()) (L1.aspBoolBOpExp ()) (L1.aspBoolExp ()) (L1.aspCondStmt ()) (L1.aspCondStmtL_Cons ()) (L1.aspCondStmtL_Nil ())
               (L1.aspIfStmt ()) (L1.aspIdExp ()) (L1.aspIntBOpExp ()) (L1.aspIntCmpExp ()) (L1.aspIntExp ()) (L1.aspSeqStmt ()) (L1.aspWhileStmt ())

mkL1' = T3.mkL1'

l1t5 = mkL1'  (L1.aspAssigStmt ()) (L1.aspBoolBOpExp ()) (L1.aspBoolExp ()) (L1.aspBoolUOpExp ()) (L1.aspCondStmt ()) (L1.aspCondStmtL_Cons ()) 
              (L1.aspCondStmtL_Nil ()) (L1.aspCstDecl ()) (L1.aspDeclL_Cons ()) (L1.aspDeclL_Nil ()) (L1.aspDecls ()) (L1.aspEmptyStmt ()) 
              (L1.aspIdExp ()) (L1.aspIdentL_Cons ()) (L1.aspIdentL_Nil ()) (L1.aspIfStmt ()) (L1.aspIntBOpExp ()) (L1.aspIntCmpExp ()) (L1.aspIntExp ()) 
              (L1.aspIntUOpExp ()) (L1.aspMaybeElseStmt_Just ()) (L1.aspMaybeElseStmt_Nothing ()) (L1.aspModule ()) (L1.aspParExp ()) 
              (L1.aspSeqStmt ()) (L1.aspTypDecl ()) (L1.aspType ()) (L1.aspVarDecl ()) (L1.aspWhileStmt ()) 



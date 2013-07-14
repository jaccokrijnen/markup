{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE TemplateHaskell, EmptyDataDecls, NoMonomorphismRestriction #-}

module Language.Oberon0.L2.SemT1 (module Language.Oberon0.L2.SemT1, module Language.Oberon0.L1.SemT1)  where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Language.Grammars.Grammar (DTerm(..), Pos(..))

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import UU.Pretty

import Language.Oberon0.L2.Decl

import qualified Language.Oberon0.L1.SemT1 as L1
import Language.Oberon0.L1.SemT1 (spp,sppl,sidl)




selfTo     = syn self $ do  return $ To
selfDownto = syn self $ do  return $ Downto


---- T1(L2) Pretty-Printing


sppNT =  nt_ForDir .*. nt_Label .*. nt_LabelL .*. L1.sppNT



sppTo     = syn spp $ do  return $ pp "TO"
sppDownto = syn spp $ do  return $ pp "DOWNTO"


sppLabelL_Cons = synmodM spp $ do  hd  <- at ch_hd_LabelL_Cons
	       	      	           tl  <- at ch_tl_LabelL_Cons
                                   let pptl = tl # spp
                                   return $ hd # spp >|< (if show pptl == ":" then empty else pp ",") >#< pptl

sppLabelL_Nil  = synmodM spp $ do  return $ pp ":"

sppExpreLbl    = synmodM spp $ do  exp <- at ch_exp_ExpreLbl
                                   return $ exp # spp

sppRangeLbl    = synmodM spp $ do  e1 <- at ch_e1_RangeLbl
                                   e2 <- at ch_e2_RangeLbl
                                   return $ e1 # spp >#< ".." >#< e2 # spp



-- sppl

spplNT =  nt_CaseL .*. nt_Case .*.  L1.spplNT

spplForStmt = synmodM sppl $ do id    <- at ch_id_ForStmt
                                start <- at ch_start_ForStmt
                                dir   <- at ch_dir_ForStmt
                                stop  <- at ch_stop_ForStmt
                                step  <- at ch_step_ForStmt
                                ss    <- at ch_ss_ForStmt
                                return $ [ "FOR" >#< value id >#< ":=" >#< 
                                           start # spp  >#<  dir # spp >#<  stop # spp >#< 
                                           "BY" >#< step # spp >#<  pp_block " DO " " END " ";" (ss # sppl)  ]


spplCaseStmt = synmodM sppl $ do exp   <- at ch_exp_CaseStmt
                                 c     <- at ch_case_CaseStmt
                                 cs    <- at ch_cases_CaseStmt
                                 e     <- at ch_else_CaseStmt
	     	                 let ppels = case (e # sppl) of
		              			[]  -> empty
		              			pps -> pp_block " ELSE " "" ";" pps
       
                                 return $ [ "CASE" >#< exp # spp >#< "OF" >-< 
                                            pp_block "" "" "|" (c # sppl ++ cs # sppl) >-<
                                            ppels >-< "END" ]

spplCase     = synmodM sppl $ do ls    <- at ch_label_Case
                                 ss    <- at ch_ss_Case
                                 return $ [ ls # spp >#< pp_block "" "" ";" (ss # sppl) ]



sppVarDecl'  = synmodM spp  $ do idl <- at ch_idl_VarDecl
                                 typ <- at ch_typ_VarDecl
                                 let ppidl = (map pp . filter (/= value caseVar)) (idl # sidl) 
                                 return $ case ppidl of
                                           []    -> empty
                                           _     -> pp_block "" "" ", " ppidl >#< ":" >#< typ # spp >|< ";"


{-
sidlIdentL_Cons'    
          = synmodM sidl $ do h <- at ch_hd_IdentL_Cons
                              t <- at ch_tl_IdentL_Cons
                              return $  if h == caseVar
                                          then (t # sidl)  
                                          else (value h) : (t # sidl)  
-}

---- Aspects

aspForStmt     _macroForStmt    = spplForStmt `ext` _macroForStmt 
aspTo                           = sppTo `ext` selfTo
aspDownto                       = sppDownto `ext` selfDownto

aspCaseStmt    _macroCaseStmt   = spplCaseStmt `ext` _macroCaseStmt    
aspCaseL_Cons  _macroCaseL_Cons = _macroCaseL_Cons 
aspCaseL_Nil   _macroCaseL_Nil  = _macroCaseL_Nil
aspCase        _macroCase       = spplCase `ext`     _macroCase 

aspLabelL_Cons  _macroLabelL_Cons = sppLabelL_Cons `ext` _macroLabelL_Cons 
aspLabelL_Nil   _macroLabelL_Nil  = sppLabelL_Nil  `ext` _macroLabelL_Nil

aspExpreLbl    _macroExpreLbl   = sppExpreLbl `ext`  _macroExpreLbl

aspRangeLbl    _macroRangeLbl   = sppRangeLbl `ext`  _macroRangeLbl



aspCst1Exp     _macroCst1Exp    = _macroCst1Exp




---- Semantic Functions

l2t1 = mkL2'   aspCase aspCaseL_Cons aspCaseL_Nil  
               aspCaseStmt aspCst1Exp aspDownto aspExpreLbl
               aspForStmt aspLabelL_Cons aspLabelL_Nil aspRangeLbl aspTo 
       
               L1.aspAssigStmt L1.aspBoolBOpExp L1.aspBoolExp L1.aspCondStmt L1.aspCondStmtL_Cons L1.aspCondStmtL_Nil
               L1.aspIfStmt L1.aspIdExp L1.aspIntBOpExp L1.aspIntCmpExp L1.aspIntExp L1.aspSeqStmt L1.aspWhileStmt

mkL1' aspAssigStmt aspBoolBOpExp aspBoolExp aspBoolUOpExp aspCondStmt aspCondStmtL_Cons 
      aspCondStmtL_Nil aspCstDecl aspDeclL_Cons aspDeclL_Nil aspDecls aspEmptyStmt 
      aspIdExp aspIdentL_Cons aspIdentL_Nil aspIfStmt aspIntBOpExp aspIntCmpExp aspIntExp 
      aspIntUOpExp aspMaybeElseStmt_Just aspMaybeElseStmt_Nothing aspModule aspParExp
      aspSeqStmt aspTypDecl aspType aspVarDecl aspWhileStmt 
  = mkL1     aspAssigStmt aspBoolBOpExp aspBoolExp aspBoolUOpExp aspCondStmt aspCondStmtL_Cons 
             aspCondStmtL_Nil aspCstDecl aspDeclL_Cons aspDeclL_Nil 
             aspDecls aspEmptyStmt 
             aspIdExp aspIdentL_Cons aspIdentL_Nil aspIfStmt aspIntBOpExp aspIntCmpExp aspIntExp 
             aspIntUOpExp aspMaybeElseStmt_Just aspMaybeElseStmt_Nothing aspModule aspParExp 
             aspSeqStmt aspTypDecl aspType (sppVarDecl' `ext` aspVarDecl) aspWhileStmt 

l1t1 = mkL1'  L1.aspAssigStmt L1.aspBoolBOpExp L1.aspBoolExp L1.aspBoolUOpExp L1.aspCondStmt L1.aspCondStmtL_Cons 
              L1.aspCondStmtL_Nil L1.aspCstDecl L1.aspDeclL_Cons L1.aspDeclL_Nil L1.aspDecls L1.aspEmptyStmt 
              L1.aspIdExp L1.aspIdentL_Cons L1.aspIdentL_Nil L1.aspIfStmt L1.aspIntBOpExp L1.aspIntCmpExp L1.aspIntExp 
              L1.aspIntUOpExp L1.aspMaybeElseStmt_Just L1.aspMaybeElseStmt_Nothing L1.aspModule L1.aspParExp 
              L1.aspSeqStmt L1.aspTypDecl L1.aspType L1.aspVarDecl L1.aspWhileStmt 



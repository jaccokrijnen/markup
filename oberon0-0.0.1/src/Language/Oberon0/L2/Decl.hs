{-# LANGUAGE TemplateHaskell, EmptyDataDecls #-}

module Language.Oberon0.L2.Decl (module Language.Oberon0.L1.Decl, module Language.Oberon0.L2.Decl) where

import Language.Oberon0.L1.Decl hiding (mkL1)
import qualified Language.Oberon0.L1.Decl as L1

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive

import Language.Grammars.Grammar(mkDTerm)

import Data.HList.Label4
import Data.HList.TypeEqGeneric1

---- L2 Grammar


data EXT_Statement 
  = ForStmt  { id_ForStmt :: String, start_ForStmt :: Expression, dir_ForStmt :: ForDir
             , stop_ForStmt :: Expression, step_ForStmt :: Expression, ss_ForStmt :: Statement }
  | CaseStmt { exp_CaseStmt :: Expression, case_CaseStmt :: Case, cases_CaseStmt :: CaseL, else_CaseStmt :: MaybeElseStmt }
    

data ForDir = To | Downto deriving Show


type CaseL  = [Case]

data Case   = Case { label_Case  ::  LabelL, ss_Case     ::  Statement }

type LabelL = [Label]

data Label  =  ExpreLbl { exp_ExpreLbl :: Expression }
            |  RangeLbl { e1_RangeLbl  :: Expression, e2_RangeLbl  :: Expression } 


$(extendAG ''EXT_Statement [''Statement, ''MaybeElseStmt, ''Expression])


data EXT_Expression  = Cst1Exp

$(extendAG ''EXT_Expression [ ])


$(deriveLang "L2" [''EXT_Statement, ''ForDir, ''CaseL, ''Case, ''LabelL, ''Label, ''EXT_Expression])

---- FOR Macro

$(attLabels ["self"])


macroForStmt aspSeqStmt aspAssigStmt aspWhileStmt aspIntCmpExp aspIdExp aspIntBOpExp
 = withChildAtt ch_dir_ForStmt self $ \dir -> 
 let  (op_stop,op_step) = case dir of 
				To     -> (LECmp,Plus)
				Downto -> (GECmp,Minus)

 in   agMacro (    aspSeqStmt
              ,    ch_s1_SeqStmt ==> (    aspAssigStmt
                                     ,    ch_id_AssigStmt  --> ch_id_ForStmt
                                     <.>  ch_exp_AssigStmt --> ch_start_ForStmt
                                     ) 
              <.>  ch_s2_SeqStmt ==> (    aspWhileStmt
                                     ,    ch_exp_WhileStmt ==> (    aspIntCmpExp
                                                               ,    ch_op_IntCmpExp ~~> op_stop
                                                               <.>  ch_e1_IntCmpExp ==> (aspIdExp, ch_id_IdExp --> ch_id_ForStmt)
                                                               <.>  ch_e2_IntCmpExp --> ch_stop_ForStmt
                                                               )
                                     <.>  ch_ss_WhileStmt  ==> (    aspSeqStmt
                                                               ,    ch_s1_SeqStmt --> ch_ss_ForStmt
                                                               <.>  ch_s2_SeqStmt ==> (    aspAssigStmt
                                                                                      ,    ch_id_AssigStmt  --> ch_id_ForStmt
                                                                                      <.>  ch_exp_AssigStmt ==> (    aspIntBOpExp
                                                                                                                ,    ch_op_IntBOpExp ~~> op_step
                                                                                                                <.>  ch_e1_IntBOpExp ==> (  aspIdExp
                                                                                                                                         ,  ch_id_IdExp   --> ch_id_ForStmt
                                                                                                                                         )
                                                                                                                <.>  ch_e2_IntBOpExp --> ch_step_ForStmt  
                                                                                                                )
                                                                                      )
                                                               )
                                     )
              <.>  ignore ch_dir_ForStmt
              )



macroCst1Exp aspIntExp = agMacro (  aspIntExp,  ch_int_IntExp ~~> (mkDTerm (1::Int))) 



-- CASE Macro


caseVar = (mkDTerm "0case") -- we use a non-valid Oberon-0 name to avoid collision  

macroCaseStmt aspSeqStmt aspAssigStmt aspIfStmt
    = agMacro (    aspSeqStmt
              ,    ch_s1_SeqStmt ==> (    aspAssigStmt
                                     ,    ch_id_AssigStmt  ~~> caseVar
                                     <.>  ch_exp_AssigStmt --> ch_exp_CaseStmt
                                     ) 
              <.>  ch_s2_SeqStmt ==> (    aspIfStmt
                                     ,    ch_if_IfStmt     --> ch_case_CaseStmt
                                     <.>  ch_elsif_IfStmt  --> ch_cases_CaseStmt 
                                     <.>  ch_else_IfStmt   --> ch_else_CaseStmt
                                     )
              )

macroCaseL_Cons aspCondStmtL_Cons
    = agMacro (    aspCondStmtL_Cons
              ,    ch_hd_CondStmtL_Cons --> ch_hd_CaseL_Cons
              <.>  ch_tl_CondStmtL_Cons --> ch_tl_CaseL_Cons
              )

macroCaseL_Nil aspCondStmtL_Nil
    = agMacro (    aspCondStmtL_Nil
              ,    noChild 
              )

macroCase aspCondStmt
    = agMacro (    aspCondStmt
              ,    ch_exp_CondStmt --> ch_label_Case
              <.>  ch_ss_CondStmt  --> ch_ss_Case
              )

macroLabelL_Cons aspBoolBOpExp
    = agMacro (    aspBoolBOpExp
              ,    ch_op_BoolBOpExp ~~> Or
              <.>  ch_e1_BoolBOpExp --> ch_hd_LabelL_Cons
              <.>  ch_e2_BoolBOpExp --> ch_tl_LabelL_Cons
              )

macroLabelL_Nil aspBoolExp
    = agMacro (    aspBoolExp
              ,    ch_bool_BoolExp ~~> (mkDTerm (False)) 
              )


macroExpreLbl aspIntCmpExp aspIdExp
    = agMacro (    aspIntCmpExp
              ,    ch_op_IntCmpExp ~~> ECmp
              <.>  ch_e1_IntCmpExp ==> (aspIdExp, ch_id_IdExp ~~> caseVar)
              <.>  ch_e2_IntCmpExp --> ch_exp_ExpreLbl
              )

macroRangeLbl aspIntCmpExp aspBoolBOpExp aspIdExp
    = agMacro (    aspBoolBOpExp
              ,    ch_op_BoolBOpExp ~~> And
              <.>  ch_e1_BoolBOpExp ==> (    aspIntCmpExp
                                        ,    ch_op_IntCmpExp ~~> GECmp
                                        <.>  ch_e1_IntCmpExp ==> (aspIdExp, ch_id_IdExp ~~> caseVar)
                                        <.>  ch_e2_IntCmpExp --> ch_e1_RangeLbl
                                        )
              <.>  ch_e2_BoolBOpExp ==> (    aspIntCmpExp
                                        ,    ch_op_IntCmpExp ~~> LECmp
                                        <.>  ch_e1_IntCmpExp ==> (aspIdExp, ch_id_IdExp ~~> caseVar)
                                        <.>  ch_e2_IntCmpExp --> ch_e2_RangeLbl
                                        )
              )



macroDeclarations  aspDeclarations aspDeclL_Cons aspVarDecl aspIdentL_Cons aspIdentL_Nil aspType
    = agMacro (    aspDeclarations
              ,    ch_cstdecl_Declarations --> ch_cstdecl_Declarations
              <.>  ch_typdecl_Declarations --> ch_typdecl_Declarations
              <.>  ch_vardecl_Declarations ==> (    aspDeclL_Cons
                                               ,    ch_hd_DeclL_Cons ==> (    aspVarDecl
                                                                         ,    ch_idl_VarDecl  ==> (    aspIdentL_Cons
                                                                                                  ,    ch_hd_IdentL_Cons ~~> caseVar
                                                                                                  <.>  ch_tl_IdentL_Cons ==> (aspIdentL_Nil, noChild)
                                                                                                  )
                                                                         <.>  ch_typ_VarDecl  ==> (    aspType
                                                                                                  ,    ch_id_Type ~~> (mkDTerm "INTEGER")
                                                                                                  )
                                                                         ) 
                                               <.>  ch_tl_DeclL_Cons -->  ch_vardecl_Declarations
                                               )
              )

 

mkL2' _aspCase _aspCaseL_Cons _aspCaseL_Nil  
      _aspCaseStmt _aspCst1Exp _aspDownto _aspExpreLbl
      _aspForStmt _aspLabelL_Cons _aspLabelL_Nil _aspRangeLbl _aspTo 
       
      _aspAssigStmt _aspBoolBOpExp _aspBoolExp _aspCondStmt _aspCondStmtL_Cons _aspCondStmtL_Nil
      _aspIfStmt _aspIdExp _aspIntBOpExp _aspIntCmpExp _aspIntExp _aspSeqStmt _aspWhileStmt

 = mkL2  (_aspCase        (macroCase         _aspCondStmt) )
         (_aspCaseL_Cons  (macroCaseL_Cons   _aspCondStmtL_Cons))
         (_aspCaseL_Nil   (macroCaseL_Nil    _aspCondStmtL_Nil))
         (_aspCaseStmt    (macroCaseStmt     _aspSeqStmt _aspAssigStmt _aspIfStmt))
         (_aspCst1Exp     (macroCst1Exp _aspIntExp))
         _aspDownto
         (_aspExpreLbl    (macroExpreLbl     _aspIntCmpExp _aspIdExp))
         (_aspForStmt (macroForStmt  _aspSeqStmt _aspAssigStmt _aspWhileStmt _aspIntCmpExp _aspIdExp _aspIntBOpExp))
         (_aspLabelL_Cons (macroLabelL_Cons  _aspBoolBOpExp))
         (_aspLabelL_Nil  (macroLabelL_Nil   _aspBoolExp))
         (_aspRangeLbl    (macroRangeLbl     _aspIntCmpExp _aspBoolBOpExp _aspIdExp))
         _aspTo

mkL1 aspAssigStmt aspBoolBOpExp aspBoolExp aspBoolUOpExp aspCondStmt aspCondStmtL_Cons 
     aspCondStmtL_Nil aspCstDecl aspDeclL_Cons aspDeclL_Nil aspDecls aspEmptyStmt 
     aspIdExp aspIdentL_Cons aspIdentL_Nil aspIfStmt aspIntBOpExp aspIntCmpExp aspIntExp 
     aspIntUOpExp aspMaybeElseStmt_Just aspMaybeElseStmt_Nothing aspModule aspParExp 
     aspSeqStmt aspTypDecl aspType aspVarDecl aspWhileStmt 
  = L1.mkL1  aspAssigStmt aspBoolBOpExp aspBoolExp aspBoolUOpExp aspCondStmt aspCondStmtL_Cons 
             aspCondStmtL_Nil aspCstDecl aspDeclL_Cons aspDeclL_Nil 
             (macroDeclarations  aspDecls aspDeclL_Cons aspVarDecl aspIdentL_Cons aspIdentL_Nil aspType) 
             aspEmptyStmt 
             aspIdExp aspIdentL_Cons aspIdentL_Nil aspIfStmt aspIntBOpExp aspIntCmpExp aspIntExp 
             aspIntUOpExp aspMaybeElseStmt_Just aspMaybeElseStmt_Nothing aspModule aspParExp 
             aspSeqStmt aspTypDecl aspType aspVarDecl aspWhileStmt 

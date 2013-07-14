{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction, TemplateHaskell, DeriveDataTypeable #-}

module Language.Oberon0.L3.SemT3 (module Language.Oberon0.L3.SemT3, module Language.Oberon0.L2.SemT3) where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Language.Grammars.Grammar (DTerm(..),Pos(..))

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import qualified Data.Map as Map
import Data.Dynamic

import qualified Language.Oberon0.L1.SemT3 as L1 -- (aspEmptyStmt, aspDeclL_Cons, aspDeclL_Nil)


import Language.Oberon0.L3.Decl

import qualified Language.Oberon0.L3.SemT2 as T2
--import L3.SemT2 (DeclDef(..))


import qualified Language.Oberon0.L2.SemT3 as L2
import Language.Oberon0.L2.SemT3 (mkL1',spp,sidl,sval,senv,ienv,serr,spos,sty,sterr,check,SymbolInfo(..),NameDef(..),TInfo(..),baseType,intType,boolType,unkType,findType)


---- T3 (L3) Type-checking

data ProcType  =  ProcType [ ParamInfo ] deriving (Typeable, Show)

data ParamInfo =  ParamInfo Pos KindParam TInfo deriving (Typeable, Show)

procType ps = let t = (ProcType ps)  
              in  TInfo (toDyn t) (show t) ((\t2 -> teq t2 unkType) . baseType)   

-- sknd
$(attLabels ["sknd"])

skndNT = nt_Expression .*. hNil

skndRule = use sknd skndNT (\_ _ -> ValP) ValP 

skndIdExp = syn sknd $ do id   <- at ch_id_IdExp
                          lhs  <- at lhs
                          return $ case (Map.lookup (value id) (lhs # ienv)) of
                                       Just (SI (NameDef _ t _) _)  -> case t of
                                                                        "Var" -> VarP
                                                                        _     -> ValP
                                       Nothing                      -> ValP


-- spl

$(attLabels ["spl"])


splNT = nt_Param .*. nt_ParamL .*. nt_ExpressionL .*. hNil

splRule = use spl splNT (++) ([] :: [ParamInfo]) 

splExpressionL_Cons = syn spl $ do  h <- at ch_hd_ExpressionL_Cons
                                    t <- at ch_tl_ExpressionL_Cons
                                    return $ (ParamInfo (h # spos) (h # sknd) (h # sty)) : (t # spl)
splExpressionL_Nil  = syn spl $ do  return $ ([] :: [ParamInfo]) 


splParam  = syn spl $ do  knd <- at ch_kind_Param
                          idl <- at ch_idl_Param
                          typ <- at ch_typ_Param
                          return $ map (const (ParamInfo (Pos 0 0) knd (typ # sty))) (idl # sidl)

-- sty

styNT = L2.styNT


-- sval

svalNT = L2.svalNT

-- senv


senvNT = nt_ParamL .*. nt_Param .*. L2.senvNT


senvParam' r    = synupdM senv $ do typ <- at ch_typ_Param
                                    return $ Map.map (\(SI nd _) -> (SI nd (SI (typ # sty) r)))

senvProcDecl' r = synupdM senv $ do id     <- at ch_id_ProcDecl
                                    params <- at ch_params_ProcDecl
                                    return $ Map.map (\(SI nd _) -> (SI nd (SI (procType (params # spl)) r)))

-- ienv

ienvNT = L2.ienvNT

stdPos = Pos (-1) (-1)
stdProcs r = Map.fromList  [ ("Read",    SI (NameDef stdPos "Prc" "standard procedure") 
                                         (SI (procType [ParamInfo stdPos VarP intType ]) r))
                           , ("Write",   SI (NameDef stdPos "Prc" "standard procedure") 
                                         (SI (procType [ParamInfo stdPos ValP intType ]) r))
                           , ("WriteLn", SI (NameDef stdPos "Prc" "standard procedure") 
                                         (SI (procType [ ]) r)) ]

ienvIni r = Map.union (L2.ienvIni r) (stdProcs r)



-- sterr

sterrNT = nt_Param  .*. nt_ParamL .*. nt_ExpressionL .*. L2.sterrNT


sterrRule = use sterr sterrNT (++) ([] :: [String]) 


checkK pos expected got = if (expected  ==  VarP) && (expected /= got) 
				then [ show pos ++ ": Illegal parameter. Variable expected"  ]  
				else []


sterrProcCStmt
          = syn sterr $ do id     <- at ch_id_ProcCStmt
                           params <- at ch_params_ProcCStmt
                           lhs    <- at lhs
                           let callpts = params # spl
                           let pid     = value id
                           return $ case (fromDynamic . trep . findType pid) (lhs # ienv) of
                                      Just (ProcType exppts) -> let  lcallpts =  length callpts
                                                                     lexppts  =  length exppts
                                                                     numerr = if lcallpts == lexppts
                                                                               then []
                                                                               else [ show (pos id) ++ ": Wrong number of parameters. Procedure " ++  pid ++
                                                                                                       " expects " ++ show lexppts ++ ", but got " ++ show lcallpts ] 
                                                                     perr = zipWith (\(ParamInfo p gk gt)  (ParamInfo _ k t) -> check p t gt ++ checkK p k gk) callpts exppts
                                                                in   numerr ++ concat perr
                                      _                      -> [] 





---- Aspects
pTInfo :: a -> SymbolInfo TInfo a
pTInfo _ = undefined

aspExtDeclarations     r = sterrRule `ext` (T2.aspExtDeclarations (pTInfo r))
aspProcDecl            r = (senvProcDecl' r) `ext` sterrRule `ext` (T2.aspProcDecl (pTInfo r)) 
aspParamL_Cons         r = splRule  `ext` sterrRule `ext` (T2.aspParamL_Cons (pTInfo r))
aspParamL_Nil          r = splRule  `ext` sterrRule `ext` (T2.aspParamL_Nil (pTInfo r))
aspParam               r = splParam `ext` (senvParam' r)  `ext` sterrRule `ext` (T2.aspParam (pTInfo r))
aspProcCStmt           r = sterrProcCStmt `ext` (T2.aspProcCStmt (pTInfo r))
aspExpressionL_Cons    r = splExpressionL_Cons `ext` sterrRule `ext` (T2.aspExpressionL_Cons (pTInfo r))
aspExpressionL_Nil     r = splExpressionL_Nil  `ext` sterrRule `ext` (T2.aspExpressionL_Nil (pTInfo r))


---- Semantic Functions

mkL1'' aspAssigStmt aspBoolBOpExp aspBoolExp aspBoolUOpExp aspCondStmt aspCondStmtL_Cons 
       aspCondStmtL_Nil aspCstDecl aspDeclL_Cons aspDeclL_Nil aspDecls aspEmptyStmt 
       aspIdExp aspIdentL_Cons aspIdentL_Nil aspIfStmt aspIntBOpExp aspIntCmpExp aspIntExp 
       aspIntUOpExp aspMaybeElseStmt_Just aspMaybeElseStmt_Nothing aspModule aspParExp
       aspSeqStmt aspTypDecl aspType aspVarDecl aspWhileStmt 
  = mkL1'    aspAssigStmt (skndRule `ext` aspBoolBOpExp) (skndRule `ext` aspBoolExp) 
             (skndRule `ext` aspBoolUOpExp) aspCondStmt aspCondStmtL_Cons 
             aspCondStmtL_Nil aspCstDecl aspDeclL_Cons aspDeclL_Nil aspDecls aspEmptyStmt 
             (skndIdExp `ext` aspIdExp) aspIdentL_Cons aspIdentL_Nil aspIfStmt (skndRule `ext` aspIntBOpExp)
             (skndRule `ext` aspIntCmpExp) (skndRule `ext` aspIntExp) 
             (skndRule `ext` aspIntUOpExp) aspMaybeElseStmt_Just aspMaybeElseStmt_Nothing aspModule 
             (skndRule `ext` aspParExp) aspSeqStmt aspTypDecl aspType aspVarDecl aspWhileStmt 


mkL2'' _aspCase _aspCaseL_Cons _aspCaseL_Nil  
       _aspCaseStmt _aspCst1Exp _aspDownto _aspExpreLbl
       _aspForStmt _aspLabelL_Cons _aspLabelL_Nil _aspRangeLbl _aspTo 
       
       _aspAssigStmt _aspBoolBOpExp _aspBoolExp _aspCondStmt _aspCondStmtL_Cons _aspCondStmtL_Nil
       _aspIfStmt _aspIdExp _aspIntBOpExp _aspIntCmpExp _aspIntExp _aspSeqStmt _aspWhileStmt

 = mkL2' _aspCase _aspCaseL_Cons _aspCaseL_Nil  
         _aspCaseStmt _aspCst1Exp _aspDownto _aspExpreLbl
         _aspForStmt _aspLabelL_Cons _aspLabelL_Nil _aspRangeLbl _aspTo 
       
         _aspAssigStmt (skndRule `ext` _aspBoolBOpExp) (skndRule `ext` _aspBoolExp) 
         _aspCondStmt _aspCondStmtL_Cons _aspCondStmtL_Nil _aspIfStmt 
         (skndRule `ext` _aspIdExp) (skndRule `ext` _aspIntBOpExp) (skndRule `ext` _aspIntCmpExp) 
         (skndRule `ext` _aspIntExp) _aspSeqStmt _aspWhileStmt  

l1t3 = mkL1'' (L1.aspAssigStmt ()) (L1.aspBoolBOpExp ()) (L1.aspBoolExp ()) (L1.aspBoolUOpExp ()) (L1.aspCondStmt ()) (L1.aspCondStmtL_Cons ()) 
              (L1.aspCondStmtL_Nil ()) (L1.aspCstDecl ()) (L1.aspDeclL_Cons ()) (L1.aspDeclL_Nil ()) (L1.aspDecls ()) (L1.aspEmptyStmt ()) 
              (L1.aspIdExp ()) (L1.aspIdentL_Cons ()) (L1.aspIdentL_Nil ()) (L1.aspIfStmt ()) (L1.aspIntBOpExp ()) (L1.aspIntCmpExp ()) (L1.aspIntExp ()) 
              (L1.aspIntUOpExp ()) (L1.aspMaybeElseStmt_Just ()) (L1.aspMaybeElseStmt_Nothing ()) (L1.aspModule ()) (L1.aspParExp ()) 
              (L1.aspSeqStmt ()) (L1.aspTypDecl ()) (L1.aspType ()) (L1.aspVarDecl ()) (L1.aspWhileStmt ()) 

l2t3 = mkL2''  (L2.aspCase ()) (L2.aspCaseL_Cons ()) (L2.aspCaseL_Nil ())  
               (L2.aspCaseStmt ()) (L2.aspCst1Exp ()) (L2.aspDownto ()) (L2.aspExpreLbl ())
               (L2.aspForStmt ()) (L2.aspLabelL_Cons ()) (L2.aspLabelL_Nil ()) (L2.aspRangeLbl ()) (L2.aspTo ()) 
       
               (L1.aspAssigStmt ()) (L1.aspBoolBOpExp ()) (L1.aspBoolExp ()) (L1.aspCondStmt ()) (L1.aspCondStmtL_Cons ()) (L1.aspCondStmtL_Nil ())
               (L1.aspIfStmt ()) (L1.aspIdExp ()) (L1.aspIntBOpExp ()) (L1.aspIntCmpExp ()) (L1.aspIntExp ()) (L1.aspSeqStmt ()) (L1.aspWhileStmt ())

l3t3 = mkL3  (aspExtDeclarations ()) (aspProcDecl ()) (aspParamL_Cons ()) (aspParamL_Nil ()) (aspParam ())     
             (aspProcCStmt ()) (aspExpressionL_Cons ()) (aspExpressionL_Nil ())
             (L1.aspEmptyStmt ()) (L1.aspDeclL_Cons ()) (L1.aspDeclL_Nil ())


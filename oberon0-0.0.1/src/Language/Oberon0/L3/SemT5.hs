{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction #-}

module Language.Oberon0.L3.SemT5 (module Language.Oberon0.L3.SemT5, module Language.Oberon0.L2.SemT5) where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Language.Grammars.Grammar (DTerm(..),Pos(..))

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import Data.List (partition)
import qualified Data.Map as Map
import Data.Dynamic

import Language.C

import qualified Language.Oberon0.L1.SemT5 as L1 -- (aspEmptyStmt, aspDeclL_Cons, aspDeclL_Nil)


import Language.Oberon0.L3.Decl

--import L3.SemT2 (DeclDef(..))


import qualified Language.Oberon0.L3.SemT3 as T3
import Language.Oberon0.L3.SemT3 (mkL1'',mkL2'',ParamInfo(..),spl,procType,ProcType(..))



import qualified Language.Oberon0.L2.SemT5 as L2
import Language.Oberon0.L2.SemT5 (spp,sidl,senv,ienv,serr,spos,sty,sterr,check,SymbolInfo(..),NameDef(..),TInfo(..),baseType,intType,boolType,unkType,findType
                ,scgen,scdecl,scty,scexp,scstmt,ipre,mapIdent,updMap,compStmt)


---- T5 (L3) Code Generation

-- ipre

ipreNT = nt_Param  .*. nt_ParamL .*. nt_ExpressionL .*. L2.ipreNT

ipreRule = copy ipre ipreNT


ipreProcDecl = inh ipre ipreNT $ do lhs     <- at lhs
                                    id      <- at ch_id_ProcDecl
                                    params  <- at ch_params_ProcDecl
                                    decls   <- at ch_decls_ProcDecl
                                    stmts   <- at ch_stmts_ProcDecl
          
                                    let  pre = (lhs # ipre) ++ "1" ++ (value id) ++ "_" 
                                    return $ ch_params_ProcDecl .=. pre .*.
                                             ch_stmts_ProcDecl  .=. pre .*.
                                             ch_decls_ProcDecl  .=. pre .*.
                                             emptyRecord 

-- senv


senvNT = T3.senvNT


senvProcDecl' r = synupdM senv $ do lhs    <- at lhs 
                                    id     <- at ch_id_ProcDecl
                                    let pid = value id
                                    return $ Map.adjust (updMap (lhs # ipre ++ pid) r) pid

senvParam' r    = synupdM senv $ do lhs <- at lhs
                                    knd <- at ch_kind_Param
                                    idl <- at ch_idl_Param
                                    let pname i =  case knd of
                                                            ValP -> updMap (lhs # ipre ++ i) r
                                                            VarP -> updMap ("(*" ++ lhs # ipre ++ i ++ ")") r
                                    return $ Map.mapWithKey pname 

-- ienv

ienvNT = L2.ienvNT

stdPos = Pos (-1) (-1)
stdProcs r = Map.fromList  [ ("Read",    SI (NameDef stdPos "Prc" "standard procedure") 
                                         (SI (procType [ParamInfo stdPos VarP intType ]) (SI "scanf" r)))
                           , ("Write",   SI (NameDef stdPos "Prc" "standard procedure") 
                                         (SI (procType [ParamInfo stdPos ValP intType ]) (SI "printf" r)))
                           , ("WriteLn", SI (NameDef stdPos "Prc" "standard procedure") 
                                         (SI (procType [ ]) (SI "printf" r))) ]


ienvIni r = Map.union (L2.ienvIni r) (stdProcs r)

-- scexp

scexpNT = L2.scexpNT

-- scexpl

$(attLabels ["scexpl"])

scexplExpressionL_Cons = syn scexpl $ do  h <- at ch_hd_ExpressionL_Cons
                                          t <- at ch_tl_ExpressionL_Cons
                                          return $ (h # scexp) : (t # scexpl)
scexplExpressionL_Nil  = syn scexpl $ do  return $ ([] :: [CExpression NodeInfo]) 


-- scdec

$(attLabels ["scdec"])

scdecNT = nt_Param .*. nt_ParamL .*. hNil

scdecRule = use scdec scdecNT (++) ([] :: [CDeclaration NodeInfo]) 

scdecParam = syn scdec $ do lhs  <- at lhs
                            kind <- at ch_kind_Param
                            idl  <- at ch_idl_Param
                            typ  <- at ch_typ_Param
                            let (tys,tyd) = typ # scty
                            return $ map (\name -> ( CDecl  [ tys ] 
                                                            [(Just (CDeclr (Just $ mapIdent name (lhs # ienv)) tyd Nothing [] undefNode),Nothing,Nothing)] 
                                                            undefNode)) 
                                         (idl # sidl)


-- scdecl

scdeclNT = L2.scdeclNT

scdeclRule = use scdecl scdeclNT (++) ([] :: [CExternalDeclaration NodeInfo]) 

filterVars = partition isNotVar


--isNotVar  (CTypeQual    (CConstQual _):_) = True
isNotVar (CDeclExt (CDecl  ts vs _)) = isNotVar' ts vs
isNotVar _                           = True

isNotVar'  ts [] = True
isNotVar'  (CStorageSpec (CTypedef   _):_) _ = True
isNotVar'  (_:ts) vs = isNotVar' ts vs
isNotVar'  []  _   = False


scdeclProcDecl = syn scdecl $ do lhs     <- at lhs
                                 id      <- at ch_id_ProcDecl
                                 params  <- at ch_params_ProcDecl
                                 decls   <- at ch_decls_ProcDecl
                                 stmts   <- at ch_stmts_ProcDecl
                                 let (nvdecs,vdecs) = filterVars (decls # scdecl)
                                 let procdef = CFDefExt $ CFunDef  [ CTypeSpec (CVoidType undefNode) ] 
                                                                   ( CDeclr  (Just $ mapIdent (value id) (lhs # ienv)) 
                                                                             [CFunDeclr (Right ((params # scdec),False)) [] undefNode] 
                                                                             Nothing [] undefNode)
                                                                   []
                                                                   (compStmt (CCompound [] (map (\(CDeclExt d)-> CBlockDecl d) vdecs) undefNode) 
                                                                             (stmts # scstmt))
                                                                   undefNode
                                 return $ nvdecs ++ [ procdef ]


-- scstmt

scstmtNT = L2.scstmtNT

scstmtProcCStmt = syn scstmt $ do lhs    <- at lhs 
                                  id     <- at ch_id_ProcCStmt
                                  params <- at ch_params_ProcCStmt
                                  let pid  = value id
                                  let desc = case pid of
                                                      "Write"   -> [CConst $ CStrConst (cString " %d") undefNode]
                                                      "WriteLn" -> [CConst $ CStrConst (cString "\n") undefNode]
                                                      "Read"    -> [CConst $ CStrConst (cString "%d") undefNode]
                                                      p         -> [] 

                                  let expps  = case (fromDynamic . trep . findType pid) (lhs # ienv) of
                                                    Just (ProcType ps) -> ps
                                                    _                  -> []
                                  let cparams = zipWith  (\(ParamInfo _ k _)  ce -> if k == VarP then CUnary CAdrOp ce undefNode else ce)
                                                        
                                  return $  CExpr (Just (CCall (CVar (mapIdent pid (lhs # ienv)) undefNode) 
                                                               (desc ++ cparams expps (params # scexpl)) 
                                                               undefNode)) 
                                                  undefNode 



---- Aspects
pMap :: a -> SymbolInfo String a
pMap _ = undefined

aspExtDeclarations     r = ipreRule `ext` scdeclRule `ext` (T3.aspExtDeclarations (pMap r))
aspProcDecl            r = ipreProcDecl `ext` (senvProcDecl' r) `ext` scdeclProcDecl `ext` (T3.aspProcDecl (pMap r)) 
aspParamL_Cons         r = ipreRule `ext` scdecRule `ext` (T3.aspParamL_Cons (pMap r))
aspParamL_Nil          r = ipreRule `ext` scdecRule `ext` (T3.aspParamL_Nil (pMap r))
aspParam               r = ipreRule `ext` (senvParam' r) `ext` scdecParam `ext` (T3.aspParam (pMap r))
aspProcCStmt           r = ipreRule `ext` scstmtProcCStmt `ext` (T3.aspProcCStmt (pMap r))
aspExpressionL_Cons    r = ipreRule `ext` scexplExpressionL_Cons `ext` (T3.aspExpressionL_Cons (pMap r))
aspExpressionL_Nil     r = ipreRule `ext` scexplExpressionL_Nil `ext` (T3.aspExpressionL_Nil (pMap r))


---- Semantic Functions

l1t5 = mkL1'' (L1.aspAssigStmt ()) (L1.aspBoolBOpExp ()) (L1.aspBoolExp ()) (L1.aspBoolUOpExp ()) (L1.aspCondStmt ()) (L1.aspCondStmtL_Cons ()) 
              (L1.aspCondStmtL_Nil ()) (L1.aspCstDecl ()) (L1.aspDeclL_Cons ()) (L1.aspDeclL_Nil ()) (L1.aspDecls ()) (L1.aspEmptyStmt ()) 
              (L1.aspIdExp ()) (L1.aspIdentL_Cons ()) (L1.aspIdentL_Nil ()) (L1.aspIfStmt ()) (L1.aspIntBOpExp ()) (L1.aspIntCmpExp ()) (L1.aspIntExp ()) 
              (L1.aspIntUOpExp ()) (L1.aspMaybeElseStmt_Just ()) (L1.aspMaybeElseStmt_Nothing ()) (L1.aspModule ()) (L1.aspParExp ()) 
              (L1.aspSeqStmt ()) (L1.aspTypDecl ()) (L1.aspType ()) (L1.aspVarDecl ()) (L1.aspWhileStmt ()) 

l2t5 = mkL2''  (L2.aspCase ()) (L2.aspCaseL_Cons ()) (L2.aspCaseL_Nil ())  
               (L2.aspCaseStmt ()) (L2.aspCst1Exp ()) (L2.aspDownto ()) (L2.aspExpreLbl ())
               (L2.aspForStmt ()) (L2.aspLabelL_Cons ()) (L2.aspLabelL_Nil ()) (L2.aspRangeLbl ()) (L2.aspTo ()) 
       
               (L1.aspAssigStmt ()) (L1.aspBoolBOpExp ()) (L1.aspBoolExp ()) (L1.aspCondStmt ()) (L1.aspCondStmtL_Cons ()) (L1.aspCondStmtL_Nil ())
               (L1.aspIfStmt ()) (L1.aspIdExp ()) (L1.aspIntBOpExp ()) (L1.aspIntCmpExp ()) (L1.aspIntExp ()) (L1.aspSeqStmt ()) (L1.aspWhileStmt ())

l3t5 = mkL3  (aspExtDeclarations ()) (aspProcDecl ()) (aspParamL_Cons ()) (aspParamL_Nil ()) (aspParam ())     
             (aspProcCStmt ()) (aspExpressionL_Cons ()) (aspExpressionL_Nil ())
             (L1.aspEmptyStmt ()) (L1.aspDeclL_Cons ()) (L1.aspDeclL_Nil ())

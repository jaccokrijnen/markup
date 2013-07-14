{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE TemplateHaskell, EmptyDataDecls, NoMonomorphismRestriction #-}

module Language.Oberon0.L1.SemT5 (module Language.Oberon0.L1.SemT5, module Language.Oberon0.L1.SemT3) where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Language.Grammars.Grammar (DTerm(..),Pos(..))

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import qualified Data.Map as Map
import Language.C

import Language.Oberon0.L1.Decl 

import qualified Language.Oberon0.L1.SemT3 as T3
import Language.Oberon0.L1.SemT3 (l1t1,l1t2,l1t3,spp,sidl,senv,ienv,serr,spos,sty,sterr,check
                ,SymbolInfo(..),NameDef(..),TInfo(..),baseType,intType,boolType,unkType,findType)

---- T5(L1) Code Generation


compStmt s1 s2 =  let  c1 = case s1 of
                                        CCompound _ cbl _ -> cbl
                                        cb                -> [ CBlockStmt cb ]
                       c2 = case s2 of
                                        CCompound _ cbl _ -> cbl
                                        cb                -> [ CBlockStmt cb ]
 
                  in   CCompound [] (c1 ++ c2) undefNode


mapIdent sid env = internalIdent $ maybe sid (\(SI _ (SI _ (SI i _))) -> i) (Map.lookup sid env) 


$(attLabels ["scgen","scdecl","scty","scexp","scstmt","ipre"])


-- ipre

ipreNT =   nt_Declarations .*. nt_DeclL .*. nt_Decl .*.
           nt_Statement .*. nt_CondStmtL .*. nt_CondStmt .*. nt_MaybeElseStmt .*. nt_Expression .*. nt_Type .*. hNil 

ipreRule = copy ipre ipreNT

-- top-level definitions prefixed by: _<module name>_ 
-- no c reserved word starts with _
ipreModule = inh ipre ipreNT $ do id <- at ch_idbgn_Module
                                  return $ ch_stmts_Module .=. ("_" ++ value id ++ "_") .*.
                                           ch_decls_Module .=. ("_" ++ value id ++ "_") .*.
                                           emptyRecord 

-- senv

senvNT = T3.senvNT

updMap ni r (SI nd (SI ti _)) = (SI nd (SI ti (SI ni r)))

senvCstDecl' r = synupdM senv $ do lhs  <- at lhs
                                   id   <- at ch_id_CstDecl
                                   let cid = value id
                                   return $ Map.adjust (updMap (lhs # ipre ++ cid) r) cid
                                                        

senvTypDecl' r = synupdM senv $ do lhs  <- at lhs
                                   id   <- at ch_id_TypDecl
                                   let tid = value id
                                   return $ Map.adjust (updMap (lhs # ipre ++ tid) r) tid
                                         

senvVarDecl' r = synupdM senv $ do lhs  <- at lhs
                                   return $ Map.mapWithKey (\k -> updMap (lhs # ipre ++ k) r)  

-- ienv

ienvNT = T3.ienvNT

ienvIni r = Map.fromList [("INTEGER", SI (NameDef (Pos (-1) (-1)) "Typ" "type")     (SI intType  (SI "int" r)))
                         ,("BOOLEAN", SI (NameDef (Pos (-1) (-1)) "Typ" "type")     (SI boolType (SI "int" r))) 
                         ,("TRUE",    SI (NameDef (Pos (-1) (-1)) "Cst" "constant") (SI boolType (SI "int" r)))
                         ,("FALSE",   SI (NameDef (Pos (-1) (-1)) "Cst" "constant") (SI boolType (SI "int" r)))] 


ienvModule' r = inhmodM ienv ienvNT $ do lhs   <- at lhs
                                         decls <- at ch_decls_Module
                                         let env = Map.union (decls # senv) (lhs # ienv)
                                         return $ ch_stmts_Module .=. env .*.
                                                  ch_decls_Module .=. env .*.
                                                  emptyRecord 

-- scgen

scgenNT =   nt_Module .*.  hNil 

scgenModule = syn scgen $ do idbgn <- at ch_idbgn_Module 
                             decls <- at ch_decls_Module
                             stmts <- at ch_stmts_Module
                             idend <- at ch_idend_Module
                             let mainfun = [ CFDefExt (CFunDef [CTypeSpec (CIntType undefNode)]
                                                               (CDeclr (Just $ (internalIdent "main")) 
                                                                       [CFunDeclr (Right ([],False)) [] undefNode] 
                                                                       Nothing [] undefNode) 
                                                               []
                                                               (compStmt (stmts # scstmt) 
                                                                         (CReturn (Just (CConst (CIntConst (cInteger 0) undefNode))) undefNode ))
                                                               undefNode) 
                                           ]
                             return $ CTranslUnit (decls # scdecl ++  mainfun) undefNode

-- scdecl

scdeclNT = nt_Declarations .*. nt_DeclL .*. nt_Decl .*. hNil

scdeclRule = use scdecl scdeclNT (++) ([] :: [CExternalDeclaration NodeInfo]) 

scdeclCstDecl = syn scdecl $ do lhs <- at lhs
                                id  <- at ch_id_CstDecl
                                exp <- at ch_exp_CstDecl
                                return [ CDeclExt $
                                         CDecl  [CTypeSpec (CEnumType (CEnum Nothing (Just [( mapIdent (value id) (lhs # ienv), Just (exp # scexp))]) 
                                                                             [] 
                                                                             undefNode) 
                                                                      undefNode)]
                                                [ ] 
                                                undefNode
                                       ]


scdeclTypDecl = syn scdecl $ do lhs <- at lhs 
                                id  <- at ch_id_TypDecl
                                typ <- at ch_typ_TypDecl
                                let (tys,tyd) = typ # scty
                                return [ CDeclExt $
                                         CDecl [CStorageSpec (CTypedef undefNode), tys]
                                               [ ( Just (CDeclr (Just $ mapIdent (value id) (lhs # ienv)) tyd Nothing [] undefNode), Nothing, Nothing)] 
                                               undefNode
                                       ] 


scdeclVarDecl = syn scdecl $ do lhs <- at lhs
                                idl <- at ch_idl_VarDecl
                                typ <- at ch_typ_VarDecl
                                let (tys,tyd) = typ # scty
                                let vars = map (\name -> (Just (CDeclr (Just $ mapIdent name (lhs # ienv)) tyd Nothing [] undefNode),Nothing,Nothing)) 
                                               (idl # sidl)
                                return [ CDeclExt $ CDecl [ tys ] vars undefNode ]

-- scty

type CType = (CTypeSpec, [CDerivedDeclr])

sctyType  = syn scty $ do  lhs <- at lhs
                           id  <- at ch_id_Type
	   	      	   return (CTypeSpec (CTypeDef (mapIdent (value id) (lhs # ienv)) undefNode), [])


-- scexp

scexpNT =   nt_Expression .*. hNil 

intCmp2C :: IntCmp -> CBinaryOp
intCmp2C ECmp   = CEqOp 
intCmp2C NECmp  = CNeqOp
intCmp2C LCmp   = CLeOp
intCmp2C LECmp  = CLeqOp
intCmp2C GCmp   = CGrOp
intCmp2C GECmp  = CGeqOp

scexpIntCmpExp = syn scexp $ do  op <- at ch_op_IntCmpExp
                                 e1 <- at ch_e1_IntCmpExp
                                 e2 <- at ch_e2_IntCmpExp
                                 return $ CBinary (intCmp2C op) (e1 # scexp) (e2 # scexp) undefNode



intBOp2C :: IntBOp -> CBinaryOp
intBOp2C Plus   = CAddOp 
intBOp2C Minus  = CSubOp
intBOp2C Times  = CMulOp
intBOp2C Div    = CDivOp
intBOp2C Mod    = CRmdOp


scexpIntBOpExp = syn scexp $ do  op <- at ch_op_IntBOpExp
                                 e1 <- at ch_e1_IntBOpExp
                                 e2 <- at ch_e2_IntBOpExp
                                 return $ CBinary (intBOp2C op) (e1 # scexp) (e2 # scexp) undefNode

intUOp2C :: IntUOp -> CUnaryOp
intUOp2C Ng = CMinOp
intUOp2C Ps = CPlusOp


scexpIntUOpExp = syn scexp $ do  op <- at ch_op_IntUOpExp
                                 e  <- at ch_e_IntUOpExp
                                 return $ CUnary (intUOp2C op) (e # scexp) undefNode


boolBOp2C :: BoolBOp -> CBinaryOp
boolBOp2C Or   = CLorOp
boolBOp2C And  = CLndOp


scexpBoolBOpExp = syn scexp $ do  op <- at ch_op_BoolBOpExp
                                  e1 <- at ch_e1_BoolBOpExp
                                  e2 <- at ch_e2_BoolBOpExp
                                  return $ CBinary (boolBOp2C op) (e1 # scexp) (e2 # scexp) undefNode


boolUOp2C :: BoolUOp -> CUnaryOp
boolUOp2C Not   = CNegOp


scexpBoolUOpExp = syn scexp $ do  op <- at ch_op_BoolUOpExp
                                  e  <- at ch_e_BoolUOpExp
                                  return $ CUnary (boolUOp2C op) (e # scexp) undefNode


scexpIdExp      = syn scexp $ do  lhs <- at lhs
                                  i   <- at ch_id_IdExp
                                  return $ CVar (mapIdent (value i) (lhs # ienv)) undefNode

scexpIntExp     = syn scexp $ do  i <- at ch_int_IntExp
                                  return $ CConst (CIntConst  (cInteger $ (toInteger . value) i) undefNode)

scexpBoolExp    = syn scexp $ do  b <- at ch_bool_BoolExp
                                  return $ CConst (CIntConst  (cInteger $ (if value b then 1 else 0)) undefNode)

scexpParExp     = syn scexp $ do  e <- at ch_e_ParExp
                                  return $ e # scexp




-- scstmt

scstmtNT = nt_Statement .*. nt_CondStmtL .*. nt_CondStmt .*. nt_MaybeElseStmt .*. hNil

scstmtAssigStmt = syn scstmt $ do lhs  <- at lhs 
                                  id   <- at ch_id_AssigStmt
                                  exp  <- at ch_exp_AssigStmt
                                  let cid = CVar (mapIdent (value id) (lhs # ienv)) undefNode
                                  return $ CExpr (Just $ CAssign CAssignOp cid (exp # scexp) undefNode) undefNode

combCIf (Just (CIf c s me n)) els = Just $ CIf c s (combCIf me els) n
combCIf Nothing               els = els 

scstmtIfStmt  = syn scstmt $ do  if_   <- at ch_if_IfStmt
                                 elsif <- at ch_elsif_IfStmt
                                 els   <- at ch_else_IfStmt
                                 return $ case (if_ # scstmt) of 
                                           CIf c s _ n ->  CIf c s (combCIf (elsif # scstmt) (els # scstmt)) n

scstmtCondStmt = syn scstmt $ do exp  <- at ch_exp_CondStmt
                                 ss   <- at ch_ss_CondStmt
                                 return $ CIf (exp # scexp) (ss # scstmt) Nothing undefNode


-- have type (Maybe CStatement)
scstmtCondStmtL_Cons = syn scstmt $ do h <- at ch_hd_CondStmtL_Cons
                                       t <- at ch_tl_CondStmtL_Cons
                                       return $  case h # scstmt of
                                                   CIf c s _ n -> Just $ CIf c s (t # scstmt) n  

scstmtCondStmtL_Nil  = syn scstmt $ do return $  Nothing


scstmtMaybeElseStmt_Just    = syn scstmt $ do j <- at ch_just_MaybeElseStmt_Just
                                              return $  Just (j # scstmt)

scstmtMaybeElseStmt_Nothing = syn scstmt $ do return $  Nothing
--


scstmtWhileStmt = syn scstmt $ do  exp  <- at ch_exp_WhileStmt
                                   ss   <- at ch_ss_WhileStmt
                                   return $ CWhile (exp # scexp) (ss # scstmt) False undefNode



scstmtSeqStmt   = syn scstmt $ do  s1  <- at ch_s1_SeqStmt
                                   s2  <- at ch_s2_SeqStmt 
                                   return $ compStmt (s1 # scstmt) (s2 # scstmt)

scstmtEmptyStmt = syn scstmt $ do  return $  CExpr Nothing undefNode



---- Aspects

pMap :: a -> SymbolInfo String a
pMap _ = undefined


aspModule                 r = (ienvModule' r) `ext` ipreModule `ext` scgenModule `ext` (T3.aspModule (pMap r))
aspDecls                  r = ipreRule `ext` scdeclRule `ext` (T3.aspDecls (pMap r))
aspDeclL_Cons         	  r = ipreRule `ext` scdeclRule `ext` (T3.aspDeclL_Cons (pMap r))
aspDeclL_Nil         	  r = ipreRule `ext` scdeclRule `ext` (T3.aspDeclL_Nil (pMap r))
aspCstDecl                r = (senvCstDecl' r) `ext` ipreRule `ext` scdeclCstDecl `ext` (T3.aspCstDecl (pMap r))
aspTypDecl                r = (senvTypDecl' r) `ext` ipreRule `ext` scdeclTypDecl `ext` (T3.aspTypDecl (pMap r))
aspVarDecl                r = (senvVarDecl' r) `ext` ipreRule `ext` scdeclVarDecl `ext` (T3.aspVarDecl (pMap r))

aspType                   r = ipreRule `ext` sctyType `ext` (T3.aspType (pMap r))

aspAssigStmt              r = ipreRule `ext` scstmtAssigStmt `ext` (T3.aspAssigStmt (pMap r))
aspIfStmt                 r = ipreRule `ext` scstmtIfStmt `ext` (T3.aspIfStmt (pMap r))
aspWhileStmt              r = ipreRule `ext` scstmtWhileStmt `ext` (T3.aspWhileStmt (pMap r))
aspSeqStmt                r = ipreRule `ext` scstmtSeqStmt `ext` (T3.aspSeqStmt (pMap r))

aspEmptyStmt              r = ipreRule `ext` scstmtEmptyStmt `ext` (T3.aspEmptyStmt (pMap r))
 
aspCondStmtL_Cons         r = ipreRule `ext` scstmtCondStmtL_Cons `ext` (T3.aspCondStmtL_Cons (pMap r))
aspCondStmtL_Nil          r = ipreRule `ext` scstmtCondStmtL_Nil `ext` (T3.aspCondStmtL_Nil (pMap r))
aspCondStmt               r = ipreRule `ext` scstmtCondStmt `ext` (T3.aspCondStmt (pMap r))

aspMaybeElseStmt_Just     r = ipreRule `ext` scstmtMaybeElseStmt_Just `ext` (T3.aspMaybeElseStmt_Just (pMap r))
aspMaybeElseStmt_Nothing  r = ipreRule `ext` scstmtMaybeElseStmt_Nothing `ext` (T3.aspMaybeElseStmt_Nothing (pMap r))


aspIntCmpExp              r = ipreRule `ext` scexpIntCmpExp `ext` (T3.aspIntCmpExp (pMap r))
aspIntBOpExp              r = ipreRule `ext` scexpIntBOpExp `ext` (T3.aspIntBOpExp (pMap r))
aspIntUOpExp              r = ipreRule `ext` scexpIntUOpExp `ext` (T3.aspIntUOpExp (pMap r))

aspBoolBOpExp             r = ipreRule `ext` scexpBoolBOpExp `ext` (T3.aspBoolBOpExp (pMap r))
aspBoolUOpExp             r = ipreRule `ext` scexpBoolUOpExp `ext` (T3.aspBoolUOpExp (pMap r))

aspIdExp                  r = ipreRule `ext` scexpIdExp `ext` (T3.aspIdExp (pMap r))

aspIntExp                 r = ipreRule `ext` scexpIntExp `ext` (T3.aspIntExp (pMap r))
aspBoolExp                r = ipreRule `ext` scexpBoolExp `ext` (T3.aspBoolExp (pMap r))
aspParExp                 r = ipreRule `ext` scexpParExp `ext` (T3.aspParExp (pMap r))   


aspIdentL_Cons            r = (T3.aspIdentL_Cons (pMap r))
aspIdentL_Nil             r = (T3.aspIdentL_Nil (pMap r))



---- Semantic Functions


l1t5 = mkL1  (aspAssigStmt ()) (aspBoolBOpExp ()) (aspBoolExp ()) (aspBoolUOpExp ()) (aspCondStmt ()) (aspCondStmtL_Cons ()) 
             (aspCondStmtL_Nil ()) (aspCstDecl ()) (aspDeclL_Cons ()) (aspDeclL_Nil ()) (aspDecls ()) (aspEmptyStmt ()) 
             (aspIdExp ()) (aspIdentL_Cons ()) (aspIdentL_Nil ()) (aspIfStmt ()) (aspIntBOpExp ()) (aspIntCmpExp ()) (aspIntExp ()) 
             (aspIntUOpExp ()) (aspMaybeElseStmt_Just ()) (aspMaybeElseStmt_Nothing ()) (aspModule ()) (aspParExp ()) 
             (aspSeqStmt ()) (aspTypDecl ()) (aspType ()) (aspVarDecl ()) (aspWhileStmt ()) 

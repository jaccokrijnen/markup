{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE TemplateHaskell, EmptyDataDecls, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Language.Oberon0.L1.SemT2 (module Language.Oberon0.L1.SemT1, module Language.Oberon0.L1.SemT2) where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Language.Grammars.Grammar (DTerm(..),Pos(..))

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import qualified Data.Map as Map


import Language.Oberon0.L1.Decl
import qualified Language.Oberon0.L1.SemT1 as T1
import Language.Oberon0.L1.SemT1 (l1t1,spp,sppl,sidl)



---- T2 (L1) Name binding

$(attLabels ["senv", "ienv", "serr", "spos"])

data SymbolInfo a b = SI a b
 deriving Show

  
data NameDef = NameDef  Pos     --position
                        String  --type of definition (i.e. Cst, Typ, Var, etc.)
                        String  --description (i.e. constant, type, variable, etc.)
 deriving Show

type NInfo a = SymbolInfo NameDef a
             
             
-- spos

sposType  = syn spos $ do  idt <- at ch_id_Type
                           return $ pos idt


-- senv

senvNT = nt_Declarations .*. nt_DeclL .*. nt_Decl .*. nt_IdentL .*.  hNil

senvRule (_ :: a) = use senv senvNT Map.union (Map.empty :: Map.Map String (NInfo a)) 


senvCstDecl  r = syn senv $ do  id  <- at ch_id_CstDecl
                                return $ Map.singleton  (value id) 
                                                        (SI (NameDef (pos id) "Cst" "constant") r)

senvTypDecl  r = syn senv $ do  id  <- at ch_id_TypDecl
                                return $ Map.singleton  (value id)
                                                        (SI (NameDef (pos id) "Typ" "type")     r)


senvIdentL_Cons r = syn senv $ do  h <- at ch_hd_IdentL_Cons
                                   t <- at ch_tl_IdentL_Cons
                                   return $ Map.insert  (value h) 
                                                        (SI (NameDef (pos h) "Var" "variable") r)
                                                        (t # senv)


-- ienv

ienvNT =  nt_Module .*. nt_Declarations .*. nt_Type .*. nt_DeclL .*. nt_Decl .*. 
          nt_Statement .*. nt_CondStmtL .*. nt_CondStmt .*. nt_MaybeElseStmt .*. nt_Expression .*. nt_IdentL .*. hNil 

ienvRule _ = copy ienv ienvNT 

ienvIni r = Map.fromList  [("INTEGER", SI (NameDef (Pos (-1) (-1)) "Typ" "type")     r)
                          ,("BOOLEAN", SI (NameDef (Pos (-1) (-1)) "Typ" "type")     r) 
                          ,("TRUE",    SI (NameDef (Pos (-1) (-1)) "Cst" "constant") r)
                          ,("FALSE",   SI (NameDef (Pos (-1) (-1)) "Cst" "constant") r)] 

ienvModule r = inh ienv ienvNT $ do lhs   <- at lhs
                                    decls <- at ch_decls_Module
                                    let env = Map.union (decls # senv) (lhs # ienv)
                                    return $ ch_stmts_Module .=. env .*.
                                             ch_decls_Module .=. env .*.
                                             emptyRecord 

ienvCstDecl _ = inh ienv ienvNT $ do lhs <- at lhs
			             id  <- at ch_id_CstDecl
                                     return $ ch_exp_CstDecl .=. ( Map.delete (value id)  .
                                                                   Map.filter (\(SI (NameDef _ knd _) _) -> knd == "Cst") ) (lhs # ienv) .*.
                                              emptyRecord

ienvTypDecl _ = inh ienv ienvNT $ do lhs <- at lhs
			             id  <- at ch_id_TypDecl
                                     return $ ch_typ_TypDecl .=. Map.delete (value id) (lhs # ienv) .*.
                                              emptyRecord

-- serr

serrNT =  nt_Module .*. nt_Declarations .*. nt_Type .*. nt_DeclL .*. nt_Decl .*.  
          nt_Statement .*. nt_CondStmtL .*. nt_CondStmt .*. nt_MaybeElseStmt .*. nt_Expression .*. nt_IdentL .*. hNil 


serrRule = use serr serrNT (++) ([] :: [String]) 

iferr cond err = if cond then err else []

serrModule = syn serr $ do idbgn <- at ch_idbgn_Module 
                           decls <- at ch_decls_Module
                           stmts <- at ch_stmts_Module
                           idend <- at ch_idend_Module
                           let err  =  show (pos idend) ++ ": " ++ value idend
                           let nerr =  iferr (value idbgn  /=  value idend) [ err ++ " sould be the Module name " ++ value idbgn ]
                           return $ nerr ++ decls # serr ++ stmts # serr


checkName name env expected place = 
          let  fid  =  value name
               err  =  show (pos name) ++ ": " ++ fid
          in   case (Map.lookup fid env) of
                    Just (SI (NameDef p t d) _)  -> iferr (not $ elem t expected) [ err ++ " is a " ++ d ++ " identifier (defined at " ++ show p 
                                                                                        ++ "), so can't be used in " ++ place ]
                    Nothing                      -> [ err ++ " is not defined" ] 

serrType  = syn serr $ do  id   <- at ch_id_Type
                           lhs  <- at lhs                         
                           return $  checkName id (lhs # ienv) ["Typ"] "a type"


serrAssigStmt = syn serr $ do id   <- at ch_id_AssigStmt
                              exp  <- at ch_exp_AssigStmt
                              lhs  <- at lhs
                              return $  checkName id (lhs # ienv) ["Var"] "an assignment" ++ exp # serr


serrIdExp = syn serr $ do id   <- at ch_id_IdExp
                          lhs <- at lhs
                          return $ checkName id (lhs # ienv) ["Var","Cst"] "an expression"


checkRep name env = 
          let  fid  =  value name
               err  =  show (pos name) ++ ": " ++ fid
          in   case (Map.lookup fid env) of
                    Just (SI (NameDef p _ _) _)  -> iferr (p /= (pos name)) [ err ++ " is already defined (at " ++ show p ++ ")" ]
                    Nothing                      -> [ ] 


serrCstDecl = syn serr $ do  id  <- at ch_id_CstDecl
                             exp <- at ch_exp_CstDecl
                             lhs <- at lhs
                             return $ checkRep id (lhs # ienv) ++ exp # serr

serrTypDecl = syn serr $ do  id  <- at ch_id_TypDecl
                             typ <- at ch_typ_TypDecl
                             lhs <- at lhs
                             return $ checkRep id (lhs # ienv) ++ typ # serr


serrIdentL_Cons = syn serr $ do  h <- at ch_hd_IdentL_Cons
                                 t <- at ch_tl_IdentL_Cons
                                 lhs <- at lhs
                                 return $ checkRep h (lhs # ienv) ++ t # serr


---- Aspects
aspModule         r        = (ienvModule r)   `ext` serrModule `ext` T1.aspModule  
aspDecls          r        = (senvRule r)     `ext` (ienvRule r)    `ext` serrRule `ext` T1.aspDecls 
aspDeclL_Cons     r        = (senvRule r)     `ext` (ienvRule r)    `ext` serrRule `ext` T1.aspDeclL_Cons
aspDeclL_Nil      r        = (senvRule r)     `ext` (ienvRule r)    `ext` serrRule `ext` T1.aspDeclL_Nil
aspCstDecl        r        = (senvCstDecl r)  `ext` (ienvCstDecl r) `ext` serrCstDecl `ext` T1.aspCstDecl
aspTypDecl        r        = (senvTypDecl r)  `ext` (ienvTypDecl r) `ext` serrTypDecl `ext` T1.aspTypDecl
aspVarDecl        r        = (senvRule r)     `ext` (ienvRule r)    `ext` serrRule    `ext` T1.aspVarDecl

aspType           r        = sposType  `ext` (ienvRule r) `ext` serrType `ext` T1.aspType
aspAssigStmt      r        = (ienvRule r) `ext` serrAssigStmt  `ext` T1.aspAssigStmt
aspIfStmt         r        = (ienvRule r) `ext` serrRule  `ext` T1.aspIfStmt
aspWhileStmt      r        = (ienvRule r) `ext` serrRule  `ext` T1.aspWhileStmt
aspSeqStmt        r        = (ienvRule r) `ext` serrRule  `ext` T1.aspSeqStmt
aspEmptyStmt      r        = (ienvRule r) `ext` serrRule  `ext` T1.aspEmptyStmt


aspCondStmtL_Cons        r  = (ienvRule r) `ext` serrRule  `ext` T1.aspCondStmtL_Cons
aspCondStmtL_Nil         r  = (ienvRule r) `ext` serrRule  `ext` T1.aspCondStmtL_Nil
aspCondStmt              r  = (ienvRule r) `ext` serrRule  `ext` T1.aspCondStmt
aspMaybeElseStmt_Just    r  = (ienvRule r) `ext` serrRule  `ext` T1.aspMaybeElseStmt_Just
aspMaybeElseStmt_Nothing r  = (ienvRule r) `ext` serrRule  `ext` T1.aspMaybeElseStmt_Nothing

aspIntCmpExp             r = (ienvRule r) `ext` serrRule   `ext` T1.aspIntCmpExp    
aspIntBOpExp             r = (ienvRule r) `ext` serrRule   `ext` T1.aspIntBOpExp
aspIntUOpExp             r = (ienvRule r) `ext` serrRule   `ext` T1.aspIntUOpExp
aspBoolBOpExp            r = (ienvRule r) `ext` serrRule   `ext` T1.aspBoolBOpExp
aspBoolUOpExp            r = (ienvRule r) `ext` serrRule   `ext` T1.aspBoolUOpExp

aspIdExp                 r = (ienvRule r) `ext` serrIdExp  `ext` T1.aspIdExp
aspIntExp                r = (ienvRule r) `ext` serrRule   `ext` T1.aspIntExp
aspBoolExp               r = (ienvRule r) `ext` serrRule   `ext` T1.aspBoolExp
aspParExp                r = (ienvRule r) `ext` serrRule   `ext` T1.aspParExp


aspIdentL_Cons           r = (senvIdentL_Cons r)  `ext` (ienvRule r) `ext`  serrIdentL_Cons  `ext`  T1.aspIdentL_Cons
aspIdentL_Nil            r = (senvRule r)         `ext` (ienvRule r) `ext`  serrRule         `ext`  T1.aspIdentL_Nil



---- Semantic Functions


l1t2 = mkL1  (aspAssigStmt ()) (aspBoolBOpExp ()) (aspBoolExp ()) (aspBoolUOpExp ()) (aspCondStmt ()) (aspCondStmtL_Cons ()) 
             (aspCondStmtL_Nil ()) (aspCstDecl ()) (aspDeclL_Cons ()) (aspDeclL_Nil ()) (aspDecls ()) (aspEmptyStmt ()) 
             (aspIdExp ()) (aspIdentL_Cons ()) (aspIdentL_Nil ()) (aspIfStmt ()) (aspIntBOpExp ()) (aspIntCmpExp ()) (aspIntExp ()) 
             (aspIntUOpExp ()) (aspMaybeElseStmt_Just ()) (aspMaybeElseStmt_Nothing ()) (aspModule ()) (aspParExp ()) 
             (aspSeqStmt ()) (aspTypDecl ()) (aspType ()) (aspVarDecl ()) (aspWhileStmt ()) 

{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE TemplateHaskell, EmptyDataDecls, NoMonomorphismRestriction, ScopedTypeVariables, DeriveDataTypeable #-}

module Language.Oberon0.L1.SemT3 (module Language.Oberon0.L1.SemT3, module Language.Oberon0.L1.SemT2) where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Language.Grammars.Grammar (DTerm(..),Pos(..))

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Dynamic

import Language.Oberon0.L1.Decl

import qualified Language.Oberon0.L1.SemT2 as T2
import Language.Oberon0.L1.SemT2 (l1t1,l1t2,spp,sidl,senv,ienv,serr,spos,SymbolInfo(..),NameDef(..),iferr)

data TInfo = TInfo { trep  :: Dynamic
                   , tshow :: String 
                   , teq   :: (TInfo -> Bool) } 
instance Show TInfo     where show  = tshow


data UnkType   =  UnkType deriving Typeable
instance Show UnkType   where show _  = "Unknown"

data IntType   =  IntType (Maybe Int) deriving Typeable
instance Show IntType   where show _  = "INTEGER"

data BoolType  =  BoolType deriving Typeable
instance Show BoolType  where show _  = "BOOLEAN"

data AliasType =  AliasType String TInfo deriving Typeable
instance Show AliasType where show (AliasType t _)  = t


elemType t = let d = toDyn t     
             in  TInfo d (show t) ((==) (dynTypeRep d) . dynTypeRep . trep . baseType)   

unkType  = TInfo (toDyn UnkType) (show UnkType) (const True)    
intType  = elemType (IntType Nothing)  
boolType = elemType BoolType

baseType t@(TInfo d _ _) = maybe t (\(AliasType _ t') -> t') (fromDynamic d)

aliasType s t = let at = AliasType s (baseType t)
                in  TInfo (toDyn at) s (checkAlias at)
                         
checkAlias (AliasType n1 t1) t2 =  maybe (teq t1 t2) (\(AliasType n2 t2') -> n1 == n2 || teq t1 t2') (fromDynamic $ trep t2)

intConst c  = elemType (IntType c)
constVal (TInfo t _ _)  = maybe Nothing (\(IntType i) -> i) (fromDynamic t) 

findType t env = maybe unkType (\(SI _ (SI ty _)) -> ty) (Map.lookup  t env)
findVal  t env = constVal (findType t env)

---- T3 (L1) Type-checking

$(attLabels ["sty", "sval", "sterr"])


-- spos

sposNT =   nt_IdentL .*. nt_Expression .*. hNil 


sposRule = use spos sposNT const (Pos 0 0) 

sposIdentL_Cons = syn spos $ do  h <- at ch_hd_IdentL_Cons
                                 return $ pos h


sposIntUOpExp   = syn spos $ do  op <- at ch_e_IntUOpExp
                                 return $ moveLeft (op # spos)

sposBoolUOpExp  = syn spos $ do  op <- at ch_e_BoolUOpExp
                                 return $ moveLeft (op # spos)


sposIdExp       = syn spos $ do  i <- at ch_id_IdExp
                                 return $ pos i

sposIntExp      = syn spos $ do  i <- at ch_int_IntExp
                                 return $ pos i

sposBoolExp     = syn spos $ do  b <- at ch_bool_BoolExp
                                 return $ pos b

sposParExp      = syn spos $ do  e <- at ch_e_ParExp
                                 return $ moveLeft (e # spos)

moveLeft (Pos     l c)    = Pos      l (c-1)
moveLeft (PosFile l c f)  = PosFile  l (c-1) f


-- sty

styNT =   nt_Expression .*. nt_Type .*. hNil 


styIntCmpExp  = syn sty $ do  return $  boolType


styIntBOpExp  = syn sty $ do  return $  intType

styIntUOpExp  = syn sty $ do  return $  intType

styBoolBOpExp = syn sty $ do  return $  boolType 


styBoolUOpExp = syn sty $ do  return $  boolType


styIdExp      = syn sty $ do  lhs  <- at lhs
                              id   <- at ch_id_IdExp
		     	      return $  findType (value id) (lhs # ienv)

styIntExp     = syn sty $ do  return $  intType

styBoolExp    = syn sty $ do  return $  boolType

styParExp     = syn sty $ do  exp   <- at ch_e_ParExp
	       		      return $  exp # sty


styType       = syn sty $ do  lhs  <- at lhs 
                              id   <- at ch_id_Type
                              return $ findType  (value id) (lhs # ienv)
                             

-- sval

svalNT =   nt_Expression .*. hNil 

svalRule = syn sval $ do  return Nothing


intBOp2Op :: IntBOp -> Int -> Int -> Int
intBOp2Op Plus   = (+) 
intBOp2Op Minus  = (-)
intBOp2Op Times  = (*)
intBOp2Op Div    = div 
intBOp2Op Mod    = mod

svalIntBOpExp = syn sval $ do  op <- at ch_op_IntBOpExp
                               e1 <- at ch_e1_IntBOpExp
                               e2 <- at ch_e2_IntBOpExp
                               return $ do v1 <- (e1 # sval)
                                           v2 <- (e2 # sval)
                                           if (op == Div || op == Mod) && (v2 == 0) 
                                            then return 0
                                            else return $ (intBOp2Op op) v1 v2

intUOp2Op :: IntUOp -> Int -> Int
intUOp2Op Ng = negate 
intUOp2Op Ps = id

svalIntUOpExp  = syn sval $ do  op <- at ch_op_IntUOpExp
                                e  <- at ch_e_IntUOpExp
                                return $ do v <- (e # sval)
                                            return $ (intUOp2Op op) v

svalIdExp   = syn sval $ do  lhs <- at lhs
                             i   <- at ch_id_IdExp
                             return $ findVal (value i) (lhs # ienv)

svalIntExp  = syn sval $ do  i <- at ch_int_IntExp
                             return $ Just (value i)

svalParExp  = syn sval $ do  e <- at ch_e_ParExp
                             return $ e # sval



-- senv

senvNT = T2.senvNT

senvCstDecl' r = synupdM senv $ do id  <- at ch_id_CstDecl
                                   exp <- at ch_exp_CstDecl
                                   return $ Map.adjust  (\(SI nd _) -> (SI nd (SI (intConst (exp # sval)) r)))  (value id)


senvTypDecl' r = synupdM senv $ do id  <- at ch_id_TypDecl
                                   typ <- at ch_typ_TypDecl
                                   let tid = value id
                                   return $ Map.adjust  (\(SI nd _) -> (SI nd (SI (aliasType tid (typ # sty)) r))) tid


senvVarDecl' r = synupdM senv $ do typ  <- at ch_typ_VarDecl
                                   return $ Map.map (\(SI nd _) -> (SI nd (SI (typ # sty) r)))


-- ienv

ienvNT =  T2.ienvNT 

ienvIni r = Map.fromList [("INTEGER", SI (NameDef (Pos (-1) (-1)) "Typ" "type")     (SI intType  r))
                         ,("BOOLEAN", SI (NameDef (Pos (-1) (-1)) "Typ" "type")     (SI boolType r)) 
                         ,("TRUE",    SI (NameDef (Pos (-1) (-1)) "Cst" "constant") (SI boolType r))
                         ,("FALSE",   SI (NameDef (Pos (-1) (-1)) "Cst" "constant") (SI boolType r))] 


-- sterr

sterrNT =  nt_Declarations .*.  nt_Type .*. nt_DeclL .*. nt_Decl .*. 
           nt_Statement .*. nt_CondStmtL .*. nt_CondStmt .*. nt_MaybeElseStmt .*. nt_Expression .*. hNil 


sterrRule = use sterr sterrNT (++) ([] :: [String]) 


check pos expected got = iferr (not $ (teq expected got) || (teq got unkType) || (teq expected unkType))
			       [ show pos ++ ": Type error. Expected " ++ show expected ++ ", but got " ++ show got ]  

checkCst pos name env  = iferr (findVal name env == Nothing)
                               [ show pos ++ ": The expression is not constant" ]
                               
checkDiv pos op v      = iferr ((op == Div || op == Mod) && (v == (Just  0)))
                               [ show pos ++ ": Division by Zero" ]  


sterrCstDecl = syn sterr $ do  id   <- at ch_id_CstDecl
                               exp  <- at ch_exp_CstDecl
                               lhs  <- at lhs
                               let err = check (exp # spos) intType (exp # sty) ++ exp # sterr
                               return $ if err == []
                                         then checkCst (exp # spos) (value id) (lhs # ienv)
                                         else err
                                     

sterrAssigStmt = syn sterr $ do  lhs  <- at lhs
                                 id   <- at ch_id_AssigStmt
                                 exp  <- at ch_exp_AssigStmt
                                 let expected = findType (value id) (lhs # ienv)
                                 return $  check (exp # spos) expected (exp # sty)  ++
                                           exp # sterr


sterrWhileStmt = syn sterr $ do  exp  <- at ch_exp_WhileStmt
                                 ss   <- at ch_ss_WhileStmt
                                 return $  check (exp # spos) boolType (exp # sty) ++ 
                                           exp # sterr ++ ss # sterr

sterrCondStmt  = syn sterr $ do  exp  <- at ch_exp_CondStmt
                                 ss   <- at ch_ss_CondStmt
                                 return $  check (exp # spos) boolType (exp # sty) ++ 
                                           exp # sterr ++ ss # sterr
 

sterrIntCmpExp = syn sterr $ do  e1 <- at ch_e1_IntCmpExp
                                 e2 <- at ch_e2_IntCmpExp
                                 return $  check (e1 # spos) intType (e1 # sty) ++ 
                                           check (e2 # spos) intType (e2 # sty) ++ 
                                           e1 # sterr ++ e2 # sterr

sterrIntBOpExp = syn sterr $ do  op <- at ch_op_IntBOpExp
                                 e1 <- at ch_e1_IntBOpExp
                                 e2 <- at ch_e2_IntBOpExp
                                 return $  checkDiv (e1 # spos) op (e2 # sval)  ++
                                           check (e1 # spos) intType (e1 # sty) ++ 
                                           check (e2 # spos) intType (e2 # sty) ++ 
                                           e1 # sterr ++ e2 # sterr

sterrIntUOpExp = syn sterr $ do  e <- at ch_e_IntUOpExp
                                 return $  check (e # spos) intType (e # sty) ++ 
                                           e # sterr


sterrBoolBOpExp = syn sterr $ do  e1 <- at ch_e1_BoolBOpExp
                                  e2 <- at ch_e2_BoolBOpExp
                                  return $  check (e1 # spos) boolType (e1 # sty) ++ 
                                            check (e2 # spos) boolType (e2 # sty) ++ 
                                            e1 # sterr ++ e2 # sterr

sterrBoolUOpExp = syn sterr $ do  e <- at ch_e_BoolUOpExp
                                  return $  check (e # spos) boolType (e # sty) ++ 
                                            e # sterr


-- serr

serrModule' = synupdM serr $ do  decls <- at ch_decls_Module
                                 stmts <- at ch_stmts_Module
                                 return $ \serr -> serr ++ decls # sterr ++ stmts # sterr

---- Aspects

pTInfo :: a -> SymbolInfo TInfo a
pTInfo _ = undefined

aspModule                 r = serrModule' `ext` (T2.aspModule (pTInfo r))  
aspDecls                  r = sterrRule `ext` (T2.aspDecls (pTInfo r))
aspDeclL_Cons             r = sterrRule `ext` (T2.aspDeclL_Cons (pTInfo r))
aspDeclL_Nil              r = sterrRule `ext` (T2.aspDeclL_Nil (pTInfo r))
aspCstDecl                r = (senvCstDecl' r) `ext` sterrCstDecl `ext` (T2.aspCstDecl (pTInfo r))
aspTypDecl                r = (senvTypDecl' r) `ext` sterrRule `ext` (T2.aspTypDecl (pTInfo r))
aspVarDecl                r = (senvVarDecl' r) `ext` sterrRule `ext` (T2.aspVarDecl (pTInfo r))

aspType                   r = styType `ext` sterrRule `ext` (T2.aspType (pTInfo r))

aspAssigStmt              r = sterrAssigStmt `ext` (T2.aspAssigStmt (pTInfo r))
aspIfStmt                 r = sterrRule `ext` (T2.aspIfStmt (pTInfo r))
aspWhileStmt              r = sterrWhileStmt `ext` (T2.aspWhileStmt (pTInfo r))
aspSeqStmt                r = sterrRule `ext` (T2.aspSeqStmt (pTInfo r))
aspEmptyStmt              r = sterrRule `ext` (T2.aspEmptyStmt (pTInfo r))
aspCondStmtL_Cons         r = sterrRule `ext` (T2.aspCondStmtL_Cons (pTInfo r))
aspCondStmtL_Nil          r = sterrRule `ext` (T2.aspCondStmtL_Nil (pTInfo r))
aspCondStmt               r = sterrCondStmt `ext` (T2.aspCondStmt (pTInfo r))
aspMaybeElseStmt_Just     r = sterrRule `ext` (T2.aspMaybeElseStmt_Just (pTInfo r))
aspMaybeElseStmt_Nothing  r = sterrRule `ext` (T2.aspMaybeElseStmt_Nothing (pTInfo r))

aspIntCmpExp              r = sposRule      `ext` styIntCmpExp  `ext` svalRule       `ext` sterrIntCmpExp  `ext` (T2.aspIntCmpExp (pTInfo r)) 
aspIntBOpExp              r = sposRule      `ext` styIntBOpExp  `ext` svalIntBOpExp  `ext` sterrIntBOpExp  `ext` (T2.aspIntBOpExp (pTInfo r)) 
aspIntUOpExp              r = sposIntUOpExp `ext` styIntUOpExp  `ext` svalIntUOpExp  `ext` sterrIntUOpExp `ext` (T2.aspIntUOpExp (pTInfo r)) 

aspBoolBOpExp             r = sposRule      `ext` styBoolBOpExp  `ext` svalRule  `ext` sterrBoolBOpExp  `ext` (T2.aspBoolBOpExp (pTInfo r)) 
aspBoolUOpExp             r = sposBoolUOpExp `ext` styBoolUOpExp `ext` svalRule  `ext` sterrBoolUOpExp  `ext` (T2.aspBoolUOpExp (pTInfo r)) 

aspIdExp                  r = sposIdExp   `ext` styIdExp   `ext` svalIdExp   `ext` sterrRule  `ext` (T2.aspIdExp (pTInfo r))   
aspIntExp                 r = sposIntExp  `ext` styIntExp  `ext` svalIntExp  `ext` sterrRule  `ext` (T2.aspIntExp (pTInfo r))   
aspBoolExp                r = sposBoolExp `ext` styBoolExp `ext` svalRule    `ext` sterrRule  `ext` (T2.aspBoolExp (pTInfo r))   
aspParExp                 r = sposParExp  `ext` styParExp  `ext` svalParExp  `ext` sterrRule  `ext` (T2.aspParExp (pTInfo r))  


aspIdentL_Cons            r = sposIdentL_Cons `ext` (T2.aspIdentL_Cons (pTInfo r))   
aspIdentL_Nil             r = sposRule        `ext` (T2.aspIdentL_Nil (pTInfo r))    


---- Semantic Functions


l1t3 = mkL1  (aspAssigStmt ()) (aspBoolBOpExp ()) (aspBoolExp ()) (aspBoolUOpExp ()) (aspCondStmt ()) (aspCondStmtL_Cons ()) 
             (aspCondStmtL_Nil ()) (aspCstDecl ()) (aspDeclL_Cons ()) (aspDeclL_Nil ()) (aspDecls ()) (aspEmptyStmt ()) 
             (aspIdExp ()) (aspIdentL_Cons ()) (aspIdentL_Nil ()) (aspIfStmt ()) (aspIntBOpExp ()) (aspIntCmpExp ()) (aspIntExp ()) 
             (aspIntUOpExp ()) (aspMaybeElseStmt_Just ()) (aspMaybeElseStmt_Nothing ()) (aspModule ()) (aspParExp ()) 
             (aspSeqStmt ()) (aspTypDecl ()) (aspType ()) (aspVarDecl ()) (aspWhileStmt ()) 

{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction, TemplateHaskell, DeriveDataTypeable #-}

module Language.Oberon0.L4.SemT3 (module Language.Oberon0.L4.SemT3, module Language.Oberon0.L3.SemT3) where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Language.Grammars.Grammar (DTerm(..),Pos(..))

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Dynamic

import Language.Oberon0.L4.Decl

import qualified Language.Oberon0.L4.SemT2 as T2

import qualified Language.Oberon0.L1.SemT3 as L1 

import qualified Language.Oberon0.L3.SemT3 as L3
import Language.Oberon0.L3.SemT3 (mkL1'',l2t3,spp,sidl,sval,senv,ienv,serr,spos,sty,sterr,sknd,spl,ParamInfo(..),check,SymbolInfo(..),NameDef(..),TInfo(..)
                ,baseType,intType,boolType,unkType,findType)


---- T3 (L4) Type-checking


data ArrType   =  ArrType (Maybe Int) TInfo
          deriving Typeable

instance Show ArrType where
 show (ArrType _ ty)  = "ARRAY OF " ++ show ty

data RecType   =  RecType [ FInfo ]
          deriving Typeable

data FInfo = FInfo String TInfo
          deriving Typeable

instance Show FInfo where
 show (FInfo f t)   = f ++ ": " ++ show t
instance Show RecType where
 show (RecType fs)  = "RECORD " ++ intercalate "; " (map show fs) ++ " END"


arrayType l ty = let t = (ArrType l ty)  
                 in  TInfo (toDyn t) (show t) (const False)   

recordType fts = let t = (RecType fts)  
                 in  TInfo (toDyn t) (show t) (const False)   

-- sknd

skndSelExp = syn sknd $ do return $ VarP

-- spos

sposSelExp = syn spos $ do  i <- at ch_id_SelExp
                            return $ pos i


-- ity

$(attLabels ["ity"])

ityNT = nt_Select  .*. nt_SelectL .*. hNil

ityRule = copy ity ityNT


ityAssigSelStmt = inh ity ityNT $ 
                            do  lhs  <- at lhs
                                id   <- at ch_id_AssigSelStmt
                                return $ ch_sel_AssigSelStmt .=. findType (value id) (lhs # ienv)  .*.
                                         emptyRecord


itySelExp = inh ity ityNT $ do  lhs  <- at lhs
                                id   <- at ch_id_SelExp
                                return $ ch_sel_SelExp .=. findType (value id) (lhs # ienv)  .*.
                                         emptyRecord

itySelectL_Cons = inh ity ityNT $ do  lhs  <- at lhs
                                      h    <- at ch_hd_SelectL_Cons
                                      return $ ch_hd_SelectL_Cons .=.  lhs # ity .*.
                                               ch_tl_SelectL_Cons .=.  h # sty   .*.
                                               emptyRecord


-- sty

styNT = nt_Select .*. nt_SelectL .*. L3.styNT

styArrayType = syn sty $ do exp  <- at ch_exp_ArrayType
                            typ  <- at ch_typ_ArrayType
                            return $ arrayType (exp # sval) (typ # sty) 

styRecordType = syn sty $ do fields  <- at ch_fields_RecordType
                             let fs = Map.toList (fields # senv)
                             return $ recordType (map (\(f, SI _ (SI t _)) -> FInfo f t) fs) 


stySelExp = syn sty $ do  sel  <- at ch_sel_SelExp
                          return $ sel # sty


fieldType _  []         =  Nothing
fieldType n ((FInfo f ft):fs) | n == f            =  Just ft
                              | otherwise         =  fieldType n fs

stySelField = syn sty $ do  lhs  <- at lhs
                            ids  <- at ch_id_SelField
 
                            return $ case (fromDynamic . trep . baseType) (lhs # ity) of
                                      Just (RecType fs)       -> maybe unkType id (fieldType (value ids) fs)
                                      _                       -> unkType


stySelArray = syn sty $ do  lhs  <- at lhs

                            return $ case (fromDynamic . trep . baseType) (lhs # ity) of
                                      Just (ArrType _ t)      -> t
                                      _                       -> unkType


stySelectL_Cons = syn sty $ do  t <- at ch_tl_SelectL_Cons
                                return $ t # sty
stySelectL_Nil  = syn sty $ do  lhs <- at lhs
                                return $ lhs # ity  

-- sval

svalNT = L3.svalNT

svalSelExp = syn sval $ do  return $ Nothing

-- senv

senvNT = T2.senvNT


senvField' r = synupdM senv $ do typ  <- at ch_typ_Field
                                 return $ Map.map (\(SI nd _) -> (SI nd (SI (typ # sty) r)))

-- ienv

ienvNT = T2.ienvNT

ienvIni = L3.ienvIni

-- serr

sterrNT = nt_Select  .*. nt_SelectL .*. nt_Field  .*. nt_FieldL .*. L3.sterrNT


sterrRule = use sterr sterrNT (++) ([] :: [String]) 

checkSize pos (Just s) | s < 0 = [ show pos ++ ": Array size is negative" ]
                       | otherwise = []
checkSize _ _ = []

sterrArrayType = syn sterr $ do exp  <- at ch_exp_ArrayType
                                typ  <- at ch_typ_ArrayType 
                                return $ check (exp # spos) intType (exp # sty) ++ checkSize (exp # spos) (exp # sval) ++ exp # sterr ++ typ # sterr  

sterrSelField = syn sterr $ do  lhs  <- at lhs
                                id   <- at ch_id_SelField

                                return $ if teq (lhs # ity) unkType 
                                          then []
                                          else case (fromDynamic . trep . baseType) (lhs # ity) of
                                                Just (RecType fs)   -> maybe [  show (pos id) ++ ": The record does not contain a field with name " ++ value id ]   
                                                                             (const []) (fieldType (value id) fs)
                                                _                   -> [  (show . L1.moveLeft) (pos id) ++ ": The accessed variable is not a record." ] 

checkIndex pos (Just i) (Just l) | i < 0 || i >= l = [  show pos ++ ": Index out of range" ]
                                 | otherwise       = [ ]
   
checkIndex _ _ _ = []


sterrSelArray = syn sterr $ do  lhs  <- at lhs
                                exp  <- at ch_exp_SelArray
                                let err = if teq (lhs # ity) unkType 
                                           then []
                                           else case (fromDynamic . trep . baseType) (lhs # ity) of
                                                 Just (ArrType l t)  -> checkIndex (exp # spos) (exp # sval) l
                                                 _                   -> [  (show . L1.moveLeft) (exp # spos) ++ ": The accessed variable is not an array." ] 

                                return $ err ++ check (exp # spos) intType (exp # sty)  

checkAssig pos ty =  let errR = case (fromDynamic . trep . baseType) ty of
                                 Just (RecType _)   -> [ show pos ++ ": Can not assign to a record." ]
                                 _                  -> [ ]
                         errA = case (fromDynamic . trep . baseType) ty of
                                 Just (ArrType _ _) -> [ show pos ++ ": Can not assign to an array." ]
                                 _                  -> [ ]
                     in errR ++ errA

sterrAssigSelStmt = syn sterr $ do  id   <- at ch_id_AssigSelStmt
                                    sel  <- at ch_sel_AssigSelStmt
                                    exp  <- at ch_exp_AssigSelStmt
                                    let lerr = checkAssig (pos id) (sel # sty) ++ (sel # sterr)
                                    return $  if lerr == []
                                               then check (exp # spos) (sel # sty) (exp # sty)  ++ exp # sterr
                                               else lerr


sterrAssigStmt' = synupdM sterr $ do  lhs  <- at lhs
                                      id   <- at ch_id_AssigStmt
                                      let lerr = checkAssig (pos id) (findType (value id) (lhs # ienv))
                                      return $ \err ->  if lerr == [] then err else lerr


checkSt _   ty VarP  = []
checkSt pos ty _     = let errR = case (fromDynamic . trep . baseType) ty of
                                    Just (RecType _)   -> [ show pos ++ ": Ilegal parameter, a record parameter has to be a variable." ]
                                    _                  -> [ ]
                           errA =  case (fromDynamic . trep . baseType) ty of
                                    Just (ArrType _ _) -> [ show pos ++ ": Ilegal parameter, an array parameter has to be a variable." ]
                                    _                  -> [ ]
                       in errR ++ errA

sterrParam' = synupdM sterr $ do  knd <- at ch_kind_Param
                                  idl <- at ch_idl_Param
                                  typ <- at ch_typ_Param
                                  return $ \err -> err ++ checkSt (idl # spos) (typ # sty) knd


---- Aspects
pTInfo :: a -> SymbolInfo TInfo a
pTInfo _ = undefined

aspArrayType                r = styArrayType  `ext` sterrArrayType `ext`  (T2.aspArrayType (pTInfo r))

aspRecordType               r = styRecordType `ext` sterrRule `ext`  (T2.aspRecordType (pTInfo r))

aspFieldL_Cons              r = sterrRule `ext`  (T2.aspFieldL_Cons (pTInfo r)) 
aspFieldL_Nil               r = sterrRule `ext`  (T2.aspFieldL_Nil (pTInfo r)) 

aspField                    r = (senvField' r) `ext` sterrRule `ext`  (T2.aspField (pTInfo r))
aspEmptyField               r = sterrRule `ext`  (T2.aspEmptyField (pTInfo r))

aspSelExp                   r = itySelExp `ext` sposSelExp `ext` stySelExp `ext`svalSelExp `ext` sterrRule `ext`  (T2.aspSelExp (pTInfo r))

aspSelectL_Cons             r = itySelectL_Cons `ext` stySelectL_Cons `ext` sterrRule `ext`  (T2.aspSelectL_Cons (pTInfo r))
aspSelectL_Nil              r = ityRule `ext` stySelectL_Nil `ext` sterrRule `ext`  (T2.aspSelectL_Nil (pTInfo r))

aspSelField                 r = ityRule `ext` stySelField `ext` sterrSelField `ext`  (T2.aspSelField (pTInfo r))
aspSelArray                 r = ityRule `ext` stySelArray `ext` sterrSelArray `ext`  (T2.aspSelArray (pTInfo r))


aspAssigSelStmt             r = ityAssigSelStmt `ext` sterrAssigSelStmt `ext`  (T2.aspAssigSelStmt (pTInfo r))

---- Semantic Functions

mkL1''' aspAssigStmt aspBoolBOpExp aspBoolExp aspBoolUOpExp aspCondStmt aspCondStmtL_Cons 
        aspCondStmtL_Nil aspCstDecl aspDeclL_Cons aspDeclL_Nil aspDecls aspEmptyStmt 
        aspIdExp aspIdentL_Cons aspIdentL_Nil aspIfStmt aspIntBOpExp aspIntCmpExp aspIntExp 
        aspIntUOpExp aspMaybeElseStmt_Just aspMaybeElseStmt_Nothing aspModule aspParExp
        aspSeqStmt aspTypDecl aspType aspVarDecl aspWhileStmt 
  = mkL1''   (sterrAssigStmt' `ext` aspAssigStmt) aspBoolBOpExp aspBoolExp 
             aspBoolUOpExp aspCondStmt aspCondStmtL_Cons 
             aspCondStmtL_Nil aspCstDecl aspDeclL_Cons aspDeclL_Nil aspDecls aspEmptyStmt 
             aspIdExp aspIdentL_Cons aspIdentL_Nil aspIfStmt aspIntBOpExp
             aspIntCmpExp aspIntExp 
             aspIntUOpExp aspMaybeElseStmt_Just aspMaybeElseStmt_Nothing aspModule 
             aspParExp aspSeqStmt aspTypDecl aspType aspVarDecl aspWhileStmt 


l1t3 = mkL1''' (L1.aspAssigStmt ()) (L1.aspBoolBOpExp ()) (L1.aspBoolExp ()) (L1.aspBoolUOpExp ()) (L1.aspCondStmt ()) (L1.aspCondStmtL_Cons ()) 
               (L1.aspCondStmtL_Nil ()) (L1.aspCstDecl ()) (L1.aspDeclL_Cons ()) (L1.aspDeclL_Nil ()) (L1.aspDecls ()) (L1.aspEmptyStmt ()) 
               (L1.aspIdExp ()) (L1.aspIdentL_Cons ()) (L1.aspIdentL_Nil ()) (L1.aspIfStmt ()) (L1.aspIntBOpExp ()) (L1.aspIntCmpExp ()) (L1.aspIntExp ()) 
               (L1.aspIntUOpExp ()) (L1.aspMaybeElseStmt_Just ()) (L1.aspMaybeElseStmt_Nothing ()) (L1.aspModule ()) (L1.aspParExp ()) 
               (L1.aspSeqStmt ()) (L1.aspTypDecl ()) (L1.aspType ()) (L1.aspVarDecl ()) (L1.aspWhileStmt ()) 

mkL3' _aspExtDeclarations _aspProcDecl _aspParamL_Cons _aspParamL_Nil _aspParam     
      _aspProcCStmt _aspExpressionL_Cons _aspExpressionL_Nil
      _aspEmptyStmt _aspDeclL_Cons _aspDeclL_Nil
   =  mkL3  _aspExtDeclarations _aspProcDecl _aspParamL_Cons _aspParamL_Nil (sterrParam' `ext` _aspParam)     
            _aspProcCStmt _aspExpressionL_Cons _aspExpressionL_Nil
            _aspEmptyStmt _aspDeclL_Cons _aspDeclL_Nil

l3t3 = mkL3' (L3.aspExtDeclarations ()) (L3.aspProcDecl ()) (L3.aspParamL_Cons ()) (L3.aspParamL_Nil ()) (L3.aspParam ())     
             (L3.aspProcCStmt ()) (L3.aspExpressionL_Cons ()) (L3.aspExpressionL_Nil ())
             (L1.aspEmptyStmt ()) (L1.aspDeclL_Cons ()) (L1.aspDeclL_Nil ())

mkL4'        _aspArrayType _aspAssigSelStmt _aspEmptyField _aspField _aspFieldL_Cons _aspFieldL_Nil
             _aspRecordType _aspSelArray _aspSelExp _aspSelField _aspSelectL_Cons _aspSelectL_Nil
  = mkL4     _aspArrayType _aspAssigSelStmt _aspEmptyField _aspField _aspFieldL_Cons _aspFieldL_Nil
             _aspRecordType _aspSelArray (skndSelExp `ext` _aspSelExp) _aspSelField _aspSelectL_Cons _aspSelectL_Nil



l4t3 = mkL4' (aspArrayType ()) (aspAssigSelStmt ()) (aspEmptyField ()) (aspField ()) (aspFieldL_Cons ()) (aspFieldL_Nil ())
             (aspRecordType ()) (aspSelArray ()) (aspSelExp ()) (aspSelField ()) (aspSelectL_Cons ()) (aspSelectL_Nil ())


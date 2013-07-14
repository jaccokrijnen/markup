{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE TemplateHaskell, EmptyDataDecls, NoMonomorphismRestriction #-}

module Language.Oberon0.L4.SemT1 (module Language.Oberon0.L4.SemT1, module Language.Oberon0.L3.SemT1) where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Language.Grammars.Grammar (DTerm(..), Pos(..))

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import UU.Pretty

import Language.Oberon0.L4.Decl


import qualified Language.Oberon0.L3.SemT1 as L3
import Language.Oberon0.L3.SemT1 (l1t1,l2t1,l3t1,spp,sppl,sidl)


---- T1(L3) Pretty-Printing


sppNT =  nt_Field .*. nt_Select .*. L3.sppNT



sppArrayType  = syn spp $ do exp   <- at ch_exp_ArrayType
                             typ   <- at ch_typ_ArrayType

                             return $ "ARRAY" >#< exp # spp >#< "OF" >#< typ # spp

sppRecordType = syn spp $ do fields  <- at ch_fields_RecordType
                             return $ pp_block " RECORD " " END " ";" (fields # sppl)

sppField   = syn spp $ do idl     <- at ch_idl_Field
                          typ     <- at ch_typ_Field
                          return $ case (map pp (idl # sidl)) of
                                    []    -> empty
                                    ppidl -> pp_block "" "" ", " ppidl >#< ":" >#< typ # spp 

sppEmptyField  = syn spp $ do return empty

sppSelExp = syn spp $ do  id   <- at ch_id_SelExp
                          sel  <- at ch_sel_SelExp
                          return $ value id >|< pp_block "" "" "" (sel # sppl)

sppSelField = syn spp $ do  id   <- at ch_id_SelField
                            return $ "." >|< value id

sppSelArray = syn spp $ do  exp  <- at ch_exp_SelArray
                            return $ "[" >|< exp # spp >|< "]"

-- sppl

spplNT = nt_FieldL .*. nt_SelectL .*. L3.spplNT


spplFieldL_Cons = syn sppl $ do  hd  <- at ch_hd_FieldL_Cons
		       	         tl  <- at ch_tl_FieldL_Cons
                                 return $ (hd # spp) : (tl # sppl)

spplFieldL_Nil  = syn sppl $ do  return $ ([] :: [PP_Doc])


spplSelectL_Cons = syn sppl $ do  h <- at ch_hd_SelectL_Cons
                                  t <- at ch_tl_SelectL_Cons
                                  return $ (h # spp) : (t # sppl)
spplSelectL_Nil  = syn sppl $ do  return $ ([] :: [PP_Doc]) 


spplAssigSelStmt = syn sppl $ do id   <- at ch_id_AssigSelStmt
                                 sel  <- at ch_sel_AssigSelStmt
                                 exp  <- at ch_exp_AssigSelStmt
                                 return $ [ value id >|< pp_block "" "" "" (sel # sppl) >#< ":=" >#< exp # spp ]

---- Aspects

aspArrayType                = sppArrayType

aspRecordType               = sppRecordType

aspFieldL_Cons              = spplFieldL_Cons 
aspFieldL_Nil               = spplFieldL_Nil 

aspField                    = sppField
aspEmptyField               = sppEmptyField

aspSelExp                   = sppSelExp

aspSelectL_Cons             = spplSelectL_Cons
aspSelectL_Nil              = spplSelectL_Nil

aspSelField                 = sppSelField
aspSelArray                 = sppSelArray


aspAssigSelStmt             = spplAssigSelStmt

---- Semantic Functions



l4t1 = mkL4  aspArrayType aspAssigSelStmt aspEmptyField aspField aspFieldL_Cons aspFieldL_Nil
             aspRecordType aspSelArray aspSelExp aspSelField aspSelectL_Cons aspSelectL_Nil


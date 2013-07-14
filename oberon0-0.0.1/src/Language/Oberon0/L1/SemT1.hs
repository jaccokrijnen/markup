{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE TemplateHaskell, EmptyDataDecls, NoMonomorphismRestriction #-}

module Language.Oberon0.L1.SemT1 where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Language.Grammars.Grammar (DTerm(..))

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import UU.Pretty

import Language.Oberon0.L1.Decl 


---- T1(L1) Pretty-Printing

instance Show IntCmp where
 show ECmp = "="; show NECmp = "#"; show LCmp = "<"; show LECmp = "<="; show GCmp = ">"; show GECmp = ">="

instance Show IntBOp where
 show Plus = "+"; show Minus = "-"; show Times = "*"; show Div = "DIV"; show Mod = "MOD"

instance Show IntUOp where
 show Ng = "-"; show Ps = "+"

instance Show BoolBOp where
 show Or = "OR"; show And = "&"

instance Show BoolUOp where
 show Not = "~"



$(attLabels ["spp","sppl","sidl"])

sppNT =   nt_Module .*. nt_Declarations .*. nt_Type .*. nt_Decl .*. nt_Expression .*. hNil 

sppRule = use spp sppNT (>-<) empty 


sppModule = syn spp $ do  idbgn <- at ch_idbgn_Module 
                          decls <- at ch_decls_Module
                          stmts <- at ch_stmts_Module
                          idend <- at ch_idend_Module

			  let ppstmts = case (stmts # sppl) of
						[]  -> empty
						pps -> pp_block "BEGIN " "" ";" pps


                          return $ "MODULE" >#< value idbgn >|< ";" >-< 
                                   decls # spp >-<
                                   ppstmts >-<
                                   "END" >#< value idend >|< "."


sppDeclarations  = syn spp $ do  cst <- at ch_cstdecl_Declarations
		  	         typ <- at ch_typdecl_Declarations
			         var <- at ch_vardecl_Declarations
			         let ppdecl decl kw = case (decl # sppl) of
						        []  -> empty
						        pps -> pp_block kw "" " " pps
                                 return $ ppdecl cst "CONST " >-< ppdecl typ "TYPE  " >-< ppdecl var "VAR   " 



sppCstDecl = syn spp $ do id  <- at ch_id_CstDecl
                          exp <- at ch_exp_CstDecl
                          return $ value id >#< "=" >#< exp # spp >|< ";"



sppTypDecl = syn spp $ do id  <- at ch_id_TypDecl
                          typ <- at ch_typ_TypDecl
                          return $ value id >#< "=" >#< typ # spp >|< ";"


sppVarDecl = syn spp $ do idl <- at ch_idl_VarDecl
                          typ <- at ch_typ_VarDecl
                          return $ case (map pp (idl # sidl)) of
                                    []    -> empty
                                    ppidl -> pp_block "" "" ", " ppidl >#< ":" >#< typ # spp >|< ";"




sppType  = syn spp $ do  id <- at ch_id_Type
                         return $ pp (value id)



sppIntCmpExp = syn spp $ do  op <- at ch_op_IntCmpExp
                             e1 <- at ch_e1_IntCmpExp
                             e2 <- at ch_e2_IntCmpExp
                             return $ e1 # spp >#< show op >#< e2 # spp

sppIntBOpExp = syn spp $ do  op <- at ch_op_IntBOpExp
                             e1 <- at ch_e1_IntBOpExp
                             e2 <- at ch_e2_IntBOpExp
                             return $ e1 # spp >#< show op >#< e2 # spp

sppIntUOpExp = syn spp $ do  op <- at ch_op_IntUOpExp
                             e  <- at ch_e_IntUOpExp
                             return $ show op >#< e # spp


sppBoolBOpExp = syn spp $ do  op <- at ch_op_BoolBOpExp
                              e1 <- at ch_e1_BoolBOpExp
                              e2 <- at ch_e2_BoolBOpExp
                              return $ e1 # spp >#< show op >#< e2 # spp

sppBoolUOpExp = syn spp $ do  op <- at ch_op_BoolUOpExp
                              e  <- at ch_e_BoolUOpExp
                              return $ show op >#< e # spp


sppIdExp  = syn spp $ do  i <- at ch_id_IdExp
                          return $ pp (value i)

sppIntExp = syn spp $ do  i <- at ch_int_IntExp
                          return $ pp (value i)

sppBoolExp = syn spp $ do  b <- at ch_bool_BoolExp
                           return $ pp (if value b then "TRUE" else "FALSE")

sppParExp = syn spp $ do  e <- at ch_e_ParExp
                          return $ "(" >|< e # spp >|< ")"



-- sppl

spplNT = nt_DeclL .*. nt_Statement .*. nt_CondStmtL .*. nt_CondStmt .*.  nt_MaybeElseStmt .*. hNil
spplRule = use sppl spplNT (++) [] 

spplDeclL_Cons = syn sppl $ do  hd  <- at ch_hd_DeclL_Cons
	  	    	        tl  <- at ch_tl_DeclL_Cons
                                return $ (hd # spp) : (tl # sppl)

spplDeclL_Nil = syn sppl $ do  return $ []


spplAssigStmt = syn sppl $ do id   <- at ch_id_AssigStmt
                              exp  <- at ch_exp_AssigStmt
                              return $ [ value id >#< ":=" >#< exp # spp ]


spplIfStmt = syn sppl $ do  if_   <- at ch_if_IfStmt
                            elsif <- at ch_elsif_IfStmt
                            els   <- at ch_else_IfStmt
                            let ppelsif = vlist $ map (" ELS" >|<) (elsif # sppl)
	 		    let ppels = case (els # sppl) of
			 		[]  -> empty
			 		pps -> pp_block " ELSE " "" ";" pps
                            return $ [ if_ # sppl >-< ppelsif >-< ppels >-< "END" ]

spplCondStmt = syn sppl $ do  exp  <- at ch_exp_CondStmt
                              ss   <- at ch_ss_CondStmt
                              return $ [  "IF" >#< exp # spp >#< 
                                          pp_block " THEN " "" ";" (ss # sppl) ]


spplWhileStmt = syn sppl $ do  exp  <- at ch_exp_WhileStmt
                               ss   <- at ch_ss_WhileStmt
                               return $ [ "WHILE" >#< exp # spp >#< 
                                          pp_block " DO " " END " ";" (ss # sppl) ]


spplSeqStmt = syn sppl $ do  s1  <- at ch_s1_SeqStmt
                             s2  <- at ch_s2_SeqStmt
                             return $ s1 # sppl ++ s2 # sppl


-- sidl

sidlNT = nt_IdentL .*. hNil
sidlRule = use sidl sidlNT (++) [] 

sidlIdentL_Cons = syn sidl $ do h <- at ch_hd_IdentL_Cons
                                t <- at ch_tl_IdentL_Cons
                                return $  (value h) : (t # sidl)  




---- Aspects
aspModule                 = sppModule 
aspDecls                  = sppDeclarations 
aspDeclL_Cons         	  = spplDeclL_Cons
aspDeclL_Nil         	  = spplDeclL_Nil
aspCstDecl                = sppCstDecl
aspTypDecl                = sppTypDecl
aspVarDecl                = sppVarDecl

aspType                   = sppType

aspAssigStmt              = spplAssigStmt 
aspIfStmt                 = spplIfStmt 
aspWhileStmt              = spplWhileStmt
aspSeqStmt                = spplRule

aspEmptyStmt              = spplRule
 
aspCondStmtL_Cons         = spplRule 
aspCondStmtL_Nil          = spplRule 
aspCondStmt               = spplCondStmt 

aspMaybeElseStmt_Just     = spplRule 
aspMaybeElseStmt_Nothing  = spplRule 


aspIntCmpExp              = sppIntCmpExp 
aspIntBOpExp              = sppIntBOpExp 
aspIntUOpExp              = sppIntUOpExp 

aspBoolBOpExp             = sppBoolBOpExp 
aspBoolUOpExp             = sppBoolUOpExp 

aspIdExp                  = sppIdExp   

aspIntExp                 = sppIntExp   
aspBoolExp                = sppBoolExp   
aspParExp                 = sppParExp   


aspIdentL_Cons            = sidlIdentL_Cons
aspIdentL_Nil             = sidlRule 



---- Semantic Functions

 
l1t1 = mkL1  aspAssigStmt aspBoolBOpExp aspBoolExp aspBoolUOpExp aspCondStmt aspCondStmtL_Cons 
             aspCondStmtL_Nil aspCstDecl aspDeclL_Cons aspDeclL_Nil aspDecls aspEmptyStmt 
             aspIdExp aspIdentL_Cons aspIdentL_Nil aspIfStmt aspIntBOpExp aspIntCmpExp aspIntExp 
             aspIntUOpExp aspMaybeElseStmt_Just aspMaybeElseStmt_Nothing aspModule aspParExp 
             aspSeqStmt aspTypDecl aspType aspVarDecl aspWhileStmt 


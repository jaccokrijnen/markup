{-# LANGUAGE TemplateHaskell, EmptyDataDecls #-}

module Language.Oberon0.L3.Decl (module Language.Oberon0.L2.Decl, module Language.Oberon0.L3.Decl) where

import Language.Oberon0.L2.Decl

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive


---- L3 Grammar

-- Procedure

type ParamL = [Param]

type GHC_KindParam = KindParam; data KindParam = VarP | ValP deriving Eq

instance Show KindParam where
 show VarP = "VAR"
 show ValP = ""

data Param 
  = Param { kind_Param ::  GHC_KindParam, idl_Param  ::  IdentL, typ_Param  ::  Type }

data EXT_Decl  
  = ProcDecl { id_ProcDecl :: String, params_ProcDecl :: ParamL, decls_ProcDecl :: Declarations
             , stmts_ProcDecl :: Statement, idend_ProcDecl :: String }

data EXT_Declarations  
  = ExtDeclarations { decls_ExtDeclarations :: Declarations, prcdecl_ExtDeclarations :: DeclL }


$(extendAG ''EXT_Decl [''Declarations,''Statement,''IdentL, ''Type])
$(extendAG ''EXT_Declarations [''Declarations,''DeclL])

type ExpressionL = [Expression]

data EXT2_Statement  
  = ProcCStmt { id_ProcCStmt :: String, params_ProcCStmt :: ExpressionL }


$(extendAG ''EXT2_Statement [''Expression])


data L3SF  ed1 ed2 ed pd1 pd2 pd3 pd4 pd5 pd plc1 plc2 pl pln
           p1 p2 p3 p pcs1 pcs2 pcs elc1 elc2 elc eln
           es dlc1 dlc2 dlc dln 
    = L3SF
     { pExtDeclarations         :: ed1 -> ed2 -> ed
     , pProcDecl                :: pd1 -> pd2 -> pd3 -> pd4 -> pd5 -> pd

     , pParamL_Cons             :: plc1 -> plc2 -> pl
     , pParamL_Nil              :: pln

     , pParam                   :: p1 -> p2 -> p3 -> p

     , pProcCStmt               :: pcs1 -> pcs2 -> pcs

     , pExpressionL_Cons        :: elc1 -> elc2 -> elc
     , pExpressionL_Nil         :: eln

     --from L1
     , pEmptyStmt'              :: es
     , pDeclL_Cons'             :: dlc1 -> dlc2 -> dlc
     , pDeclL_Nil'              :: dln

     }



mkL3 _aspExtDeclarations _aspProcDecl _aspParamL_Cons _aspParamL_Nil _aspParam     
     _aspProcCStmt _aspExpressionL_Cons _aspExpressionL_Nil

     _aspEmptyStmt _aspDeclL_Cons _aspDeclL_Nil

   = L3SF
     { pExtDeclarations          = semP_ExtDeclarations   _aspExtDeclarations 
     , pProcDecl                 = semP_ProcDecl          _aspProcDecl

     , pParamL_Cons              = semP_ParamL_Cons       _aspParamL_Cons
     , pParamL_Nil               = semP_ParamL_Nil        _aspParamL_Nil 

     , pParam                    = semP_Param             _aspParam     

     , pProcCStmt                = semP_ProcCStmt         _aspProcCStmt

     , pExpressionL_Cons         = semP_ExpressionL_Cons  _aspExpressionL_Cons
     , pExpressionL_Nil          = semP_ExpressionL_Nil   _aspExpressionL_Nil

     --from L1
     , pEmptyStmt'               = semP_EmptyStmt         _aspEmptyStmt
     , pDeclL_Cons'              = semP_DeclL_Cons        _aspDeclL_Cons
     , pDeclL_Nil'               = semP_DeclL_Nil         _aspDeclL_Nil
 
     }


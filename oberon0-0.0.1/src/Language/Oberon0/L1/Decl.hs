{-# LANGUAGE TemplateHaskell, EmptyDataDecls #-}

module Language.Oberon0.L1.Decl where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive

---- L1 Grammar
data Module 
   = Module  { idbgn_Module :: String, decls_Module :: Declarations, stmts_Module :: Statement, idend_Module :: String }


data Declarations 
   = Declarations  { cstdecl_Declarations ::  DeclL, typdecl_Declarations ::  DeclL, vardecl_Declarations ::  DeclL }


type DeclL = [ Decl ]

data Decl 
   = CstDecl  { id_CstDecl  :: String, exp_CstDecl :: Expression }
   | TypDecl  { id_TypDecl  :: String, typ_TypDecl :: Type }
   | VarDecl  { idl_VarDecl :: IdentL, typ_VarDecl :: Type }

data Type
   = Type     { id_Type :: String }


data Statement
   = AssigStmt  { id_AssigStmt  :: String,     exp_AssigStmt  :: Expression }
   | IfStmt     { if_IfStmt     :: CondStmt,   elsif_IfStmt   :: CondStmtL, else_IfStmt    :: MaybeElseStmt  }
   | WhileStmt  { exp_WhileStmt :: Expression, ss_WhileStmt   :: Statement }
   | SeqStmt	{ s1_SeqStmt    :: Statement,  s2_SeqStmt     :: Statement }
   | EmptyStmt


type CondStmtL = [ CondStmt ]

data CondStmt
   = CondStmt   { exp_CondStmt  :: Expression, ss_CondStmt   :: Statement }

type MaybeElseStmt = Maybe Statement


type IdentL = [ String ]

type GHC_IntCmp = IntCmp; data IntCmp = ECmp | NECmp | LCmp | LECmp | GCmp | GECmp deriving Eq
type GHC_IntBOp = IntBOp; data IntBOp = Plus | Minus | Times | Div | Mod deriving Eq
type GHC_IntUOp = IntUOp; data IntUOp = Ng | Ps deriving Eq


type GHC_BoolBOp = BoolBOp; data BoolBOp = Or | And deriving Eq
type GHC_BoolUOp = BoolUOp; data BoolUOp = Not deriving Eq

data Expression
   = IntCmpExp   { op_IntCmpExp   :: GHC_IntCmp, e1_IntCmpExp   :: Expression, e2_IntCmpExp   :: Expression }
   | IntBOpExp   { op_IntBOpExp   :: GHC_IntBOp, e1_IntBOpExp   :: Expression, e2_IntBOpExp   :: Expression }
   | IntUOpExp   { op_IntUOpExp   :: GHC_IntUOp, e_IntUOpExp    :: Expression }

   | BoolBOpExp  { op_BoolBOpExp  :: GHC_BoolBOp, e1_BoolBOpExp  :: Expression, e2_BoolBOpExp  :: Expression }
   | BoolUOpExp  { op_BoolUOpExp  :: GHC_BoolUOp, e_BoolUOpExp   :: Expression }

   | IdExp       { id_IdExp       :: String }
   | IntExp      { int_IntExp     :: Int }
   | BoolExp     { bool_BoolExp   :: Bool }
   | ParExp      { e_ParExp       :: Expression }


$(deriveAG ''Module)

$(addNT "Ident")
$(addNT "StatementSequence")

$(deriveLang "L1" [''Module, ''Declarations, ''DeclL, ''Decl, ''Type, ''Statement, ''CondStmtL, ''CondStmt, ''MaybeElseStmt, ''Expression, ''IdentL])


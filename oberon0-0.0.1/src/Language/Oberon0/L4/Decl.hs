{-# LANGUAGE TemplateHaskell, EmptyDataDecls #-}

module Language.Oberon0.L4.Decl (module Language.Oberon0.L3.Decl, module Language.Oberon0.L4.Decl) where

import Language.Oberon0.L3.Decl

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive


---- L4 Grammar

-- Procedure

data EXT_Type  = ArrayType  { exp_ArrayType :: Expression, typ_ArrayType :: Type }
               | RecordType { fields_RecordType :: FieldL }

type FieldL = [Field]


data Field  = Field { idl_Field  :: IdentL, typ_Field  :: Type }
            | EmptyField



$(extendAG ''EXT_Type [''Expression,''IdentL, ''Type])


data EXT2_Expression = SelExp { id_SelExp :: String, sel_SelExp :: SelectL }

type SelectL = [Select]


data Select  = SelField { id_SelField  :: String }
             | SelArray { exp_SelArray :: Expression }


$(extendAG ''EXT2_Expression [''Expression])


data EXT3_Statement 
  = AssigSelStmt { id_AssigSelStmt :: String, sel_AssigSelStmt :: SelectL, exp_AssigSelStmt :: Expression }

$(extendAG ''EXT3_Statement [''SelectL,''Expression])

$(deriveLang "L4" [''EXT_Type, ''FieldL, ''Field, ''EXT2_Expression, ''SelectL, ''Select, ''EXT3_Statement])


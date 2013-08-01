{-# LANGUAGE TemplateHaskell, EmptyDataDecls #-}
module Semantics.HTML where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive

import Document

$(deriveAG ''Root)
$(attLabels ["shtml"])

shtmlPlain = syn shtml $ do at ch_str_plainInl


{-# LANGUAGE Arrows, DoRec, TemplateHaskell, EmptyDataDecls #-}

module Test (
	module Language.Grammars.Murder.UUParsing,
	primitives,
	parser)

	where

import Control.Applicative

import Language.Grammars.Grammar
import Language.Grammars.Murder
import Language.Grammars.Murder.Derive
import Language.Grammars.Murder.UUParsing

$(csLabels  ["cs_Root", "cs_Other"])

-- | A grammar with common lexical structures for 
primitives = proc () -> do
	
	rec 
		root  <- addNT -< semPlus <$> sym int <* tr "+" <*> sym int
		other <- addNT -< sym (anyof "abcd") <* tr "!"
	exportNTs -< exportList root (export cs_Root root . export cs_Other other)


semPlus (DTerm _ x) (DTerm _ y) = x + y


parser = compile (closeGram primitives)
{-# LANGUAGE  EmptyDataDecls #-}

module Utils where

import qualified Data.Set as Set

import UU.Pretty

import Language.Grammars.Murder
import Language.Grammars.Grammar

import Language.Grammars.Murder.Scanner

extKeywordsTxt ::  ScanOpts -> [String] -> ScanOpts
extKeywordsTxt opts kws = opts { scoKeywordsTxt = scoKeywordsTxt opts  `Set.union` Set.fromList kws }

extSpecChars ::  ScanOpts -> [Char] -> ScanOpts
extSpecChars opts kws = opts { scoSpecChars = scoSpecChars opts  `Set.union` Set.fromList kws }

data NTRoot
ntRoot = undefined :: NTRoot

data NTExp
ntExp = undefined :: NTExp

data NTTerm
ntTerm = undefined :: NTTerm

data NTFactor
ntFactor = undefined :: NTFactor

trm = Term


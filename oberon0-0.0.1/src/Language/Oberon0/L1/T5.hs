{-# OPTIONS -fcontext-stack=1000 #-}

import Language.Oberon0.L1.Gram
import Language.Oberon0.L1.SemT5
import Language.Oberon0.MainT5

import Language.Grammars.Murder
import Language.Grammars.Murder.UUParsing

import Language.Grammars.AspectAG


c_l1t5 = closeGram (l1 l1t5)


main = mainT5 (# spp) (# serr) (# scgen) (ienv .=. ienvIni () .*. emptyRecord) "l1t5" l1Kws c_l1t5



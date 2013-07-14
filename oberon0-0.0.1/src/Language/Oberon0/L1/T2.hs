{-# OPTIONS -fcontext-stack=1000 #-}

import Language.Oberon0.L1.Gram
import Language.Oberon0.L1.SemT2
import Language.Oberon0.MainT2

import Language.Grammars.Murder
import Language.Grammars.Murder.UUParsing

import Language.Grammars.AspectAG


c_l1t2 = closeGram (l1 l1t2)


main = mainT2 (# spp) (# serr) (ienv .=. ienvIni () .*. emptyRecord) "l1t2" l1Kws c_l1t2


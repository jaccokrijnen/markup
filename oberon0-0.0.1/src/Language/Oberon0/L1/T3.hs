{-# OPTIONS -fcontext-stack=1000 #-}

import Language.Oberon0.L1.Gram
import Language.Oberon0.L1.SemT3
import Language.Oberon0.MainT2

import Language.Grammars.Murder
import Language.Grammars.AspectAG


c_l1t3 = closeGram (l1 l1t3)


main = mainT2 (# spp) (# serr) (ienv .=. ienvIni () .*. emptyRecord) "l1t3" l1Kws c_l1t3


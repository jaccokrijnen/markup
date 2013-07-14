{-# OPTIONS -fcontext-stack=3000 #-}

import Language.Oberon0.L3.Gram
import Language.Oberon0.L3.SemT5
import Language.Oberon0.MainT5

import Language.Grammars.Murder
import Language.Grammars.AspectAG


c_l3t5 = closeGram (l1 l1t5 +>> l2 l2t5 +>> l3 l3t5)


main = mainT5 (# spp) (# serr) (# scgen) (ienv .=. ienvIni () .*. emptyRecord) "l3t5" l3Kws c_l3t5



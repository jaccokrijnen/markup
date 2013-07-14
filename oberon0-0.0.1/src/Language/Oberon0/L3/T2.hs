{-# OPTIONS -fcontext-stack=1000 #-}

import Language.Oberon0.L3.Gram
import Language.Oberon0.L3.SemT2
import Language.Oberon0.MainT2

import Language.Grammars.Murder
import Language.Grammars.AspectAG


c_l3t2 = closeGram (l1 l1t2 +>> l2 l2t2 +>> l3 l3t2)


main = mainT2 (# spp) (# serr) (ienv .=. ienvIni () .*. emptyRecord) "l3t2" l3Kws c_l3t2


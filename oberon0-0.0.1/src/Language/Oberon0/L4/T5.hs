{-# OPTIONS -fcontext-stack=1000 #-}

import Language.Oberon0.L4.Gram
import Language.Oberon0.L4.SemT5
import Language.Oberon0.MainT5

import Language.Grammars.Murder
import Language.Grammars.AspectAG


c_l4t5 = closeGram (l1 l1t5 +>> l2 l2t5 +>> l3 l3t5 +>> l4 l4t5)


main = mainT5 (# spp) (# serr) (# scgen) (ienv .=. ienvIni () .*. emptyRecord) "l4t5" l4Kws c_l4t5



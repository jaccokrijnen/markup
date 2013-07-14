{-# OPTIONS -fcontext-stack=1000 #-}

import Language.Oberon0.L2.Gram
import Language.Oberon0.L2.SemT3
import Language.Oberon0.MainT2

import Language.Grammars.Murder
import Language.Grammars.AspectAG


c_l2t3 = closeGram (l1 l1t3 +>> l2 l2t3)


main = mainT2 (# spp) (# serr) (ienv .=. ienvIni () .*. emptyRecord) "l2t3" l2Kws c_l2t3


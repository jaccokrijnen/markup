{-# OPTIONS -fcontext-stack=1000 #-}

import Language.Oberon0.MainT2
import Language.Oberon0.L4.Gram
import Language.Oberon0.L4.SemT2


import Language.Grammars.Murder
import Language.Grammars.AspectAG


c_l4t2 = closeGram (l1 l1t2 +>> l2 l2t2 +>> l3 l3t2 +>> l4 l4t2)

main = mainT2 (# spp) (# serr) (ienv .=. ienvIni () .*. emptyRecord) "l4t2" l4Kws c_l4t2

{-# OPTIONS -fcontext-stack=1000 #-}

import Language.Oberon0.MainT1
import Language.Oberon0.L3.Gram
import Language.Oberon0.L3.SemT1


import Language.Grammars.Murder
import Language.Grammars.AspectAG


c_l3t1 = closeGram (l1 l1t1 +>> l2 l2t1 +>> l3 l3t1)

main = mainT1 (# spp) emptyRule "l3t1" l3Kws c_l3t1

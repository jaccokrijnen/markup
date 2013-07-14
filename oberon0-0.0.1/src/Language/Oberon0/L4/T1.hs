{-# OPTIONS -fcontext-stack=1000 #-}

import Language.Oberon0.MainT1
import Language.Oberon0.L4.Gram
import Language.Oberon0.L4.SemT1


import Language.Grammars.Murder
import Language.Grammars.AspectAG


c_l4t1 = closeGram (l1 l1t1 +>> l2 l2t1 +>> l3 l3t1 +>> l4 l4t1)

main = mainT1 (# spp) emptyRule "l4t1" l4Kws c_l4t1

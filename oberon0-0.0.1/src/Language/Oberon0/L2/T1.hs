{-# OPTIONS -fcontext-stack=1000 #-}

import Language.Oberon0.MainT1
import Language.Oberon0.L2.Gram
import Language.Oberon0.L2.SemT1


import Language.Grammars.Murder
import Language.Grammars.AspectAG


c_l2t1 = closeGram (l1 l1t1 +>> l2 l2t1)

main = mainT1 (# spp) emptyRule "l2t1" l2Kws c_l2t1

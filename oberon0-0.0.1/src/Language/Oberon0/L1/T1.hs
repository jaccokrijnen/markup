{-# OPTIONS -fcontext-stack=1000 #-}

import Language.Oberon0.MainT1
import Language.Oberon0.L1.Gram
import Language.Oberon0.L1.SemT1


import Language.Grammars.Murder
import Language.Grammars.AspectAG


c_l1t1 = closeGram (l1 l1t1)

main = mainT1 (# spp) emptyRecord "l1t1" l1Kws c_l1t1

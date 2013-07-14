{-# OPTIONS -fcontext-stack=1000 #-}

import Language.Oberon0.L2.Gram
import Language.Oberon0.L2.SemT2
import Language.Oberon0.MainT2

import Language.Grammars.Murder
import Language.Grammars.Murder.UUParsing

import Language.Grammars.AspectAG


c_l2t2 = closeGram (l1 l1t2 +>> l2 l2t2)


main = mainT2 (# spp) (# serr) (ienv .=. ienvIni () .*. emptyRecord) "l2t2" l2Kws c_l2t2


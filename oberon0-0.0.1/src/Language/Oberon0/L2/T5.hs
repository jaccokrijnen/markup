{-# OPTIONS -fcontext-stack=1000 #-}

import Language.Oberon0.L2.Gram
import Language.Oberon0.L2.SemT5
import Language.Oberon0.MainT5

import Language.Grammars.Murder
import Language.Grammars.Murder.UUParsing

import Language.Grammars.AspectAG


c_l2t5 = closeGram (l1 l1t5 +>> l2 l2t5)


main = mainT5 (# spp) (# serr) (# scgen) (ienv .=. ienvIni () .*. emptyRecord) "l2t5" l2Kws c_l2t5



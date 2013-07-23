{-# LANGUAGE  RankNTypes, Arrows, DoRec #-} 

module Language.Grammars.Transformations.RemoveFix (removeFix) where


import Language.AbstractSyntax.TTTAS
import Language.Grammars.Grammar
import Language.Grammars.Transformations.GramTrafo

import Control.Arrow


removeFix :: forall env a. Ref a env -> PreGramEnv env env -> Grammar a
removeFix start productions
      = case runTrafo (remfixtrafo productions) Unit () of
            Result _ (T tt) gram -> 
                 Grammar (tt start) gram

remfixtrafo :: PreGramEnv env env 
        -> Trafo Unit (Productions NF) s () (T env s)
remfixtrafo productions = proc _ ->
            do   rec  let tenv_s = map2trans menv_s
                      menv_s <- (remfixProds productions) -< tenv_s
                 returnA -< tenv_s


remfixProds  ::  PreGramEnv env env' 
             ->  Trafo  Unit (Productions NF) s (T env s) 
                                                (Mapping env' s)
remfixProds Empty          
     = proc _ ->
        returnA -< Mapping Empty  

remfixProds (Ext p (PS prods)) 
     = proc tenv_s ->
        do  ps <- sequenceA  (map  remfixProd prods) -< tenv_s 
            r  <- newSRef -< PS ps   
            Mapping e <- remfixProds p -< tenv_s
            returnA -< Mapping (Ext e r)



remfixProd  :: Prod TL a env 
            -> Trafo Unit (Productions NF) s (T env s) (Prod NF a s)

remfixProd (Fix (PS ps)) 
     = proc tenv_s ->
        do  rec  r   <- newSRef -<  PS (map (remVar r tenv_s) ps)     
            returnA -< (Sym $ Nont r)

remfixProd (Star f g) 
     = proc tenv_s ->
        do  f' <- remfixProd f -< tenv_s
            g' <- remfixProd g -< tenv_s
            returnA -< Star f' g'
{-
remfixProd (FlipStar f g) 
     = proc tenv_s ->
        do  f' <- remfixProd f -< tenv_s
            g' <- remfixProd g -< tenv_s
            returnA -< FlipStar f' g'
-}
remfixProd (Sym s) 
     = proc tenv_s ->
        do  returnA -< Sym $ mapSym tenv_s s

remfixProd (Pure x) 
     = proc _ ->
        do  returnA -< Pure x


mapSym :: T env1 env2 -> Symbol a t env1 -> Symbol a t env2
mapSym t (Nont x)      = Nont (unT t x)
mapSym _ (Term x)      = Term x
mapSym _ TermInt       = TermInt 
mapSym _ TermChar      = TermChar 
mapSym _ TermVarid     = TermVarid 
mapSym _ TermConid     = TermConid 
mapSym _ TermOp        = TermOp 
mapSym _ (TermAnyOf x) = TermAnyOf x

remVar  :: Ref b s -> T env s -> Prod (FL b) a env -> Prod NF a s
remVar _  t  (Sym s)             = Sym $ mapSym t s 
remVar _  _  (Pure x)            = Pure x
remVar r  t  (Star f g)          = Star (remVar r t f) (remVar r t g)
remVar r  _  Var                 = Sym (Nont r) 


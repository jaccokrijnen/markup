{-# LANGUAGE RankNTypes, GADTs #-}
module Language.Grammars.Transformations.GramTrafo where

import Language.AbstractSyntax.TTTAS
import Language.Grammars.Grammar

data MapA_X env a env'
  = MapA_X (forall x t. Symbol (x->a)  t env -> Maybe (Ref x       env')) -- Star
           (forall x t. Symbol x       t env -> Maybe (Ref (x->a)  env')) -- Flip


emptyMap :: MapA_X env a env'
emptyMap  = MapA_X (const Nothing) (const Nothing)

extendMapS :: Symbol (x->a) t env -> MapA_X env a env'
           -> MapA_X env a (env',x)
extendMapS  x (MapA_X ms mf) 
        = MapA_X  (\s -> case matchSym s x of
                                     Just Eq -> Just Zero
                                     Nothing -> fmap Suc (ms s))
                  (\s -> fmap Suc (mf s))

extendMapF :: Symbol x t env -> MapA_X env a env'
           -> MapA_X env a (env',x->a)
extendMapF  x (MapA_X ms mf) 
        = MapA_X  (\s -> fmap Suc (ms s))
                  (\s -> case matchSym s x of
                                     Just Eq -> Just Zero
                                     Nothing -> fmap Suc (mf s))
                 


type GramTrafo env a = Trafo (MapA_X env a) (Productions NF)


initMap :: GramTrafo env a s c d
        -> Trafo Unit (Productions NF) s c d
initMap (Trafo st) 
        = Trafo (\_ -> case st emptyMap of
                            TrafoE _ f -> TrafoE Unit f
                )

newNontRS  ::  forall x t env s a 
           .   Symbol (x->a) t env 
           ->  GramTrafo env a s (Productions NF x s) (Ref x s)
newNontRS x = Trafo $ \m -> extEnv (extendMapS x m)

newNontRF  ::  forall x t env s a 
           .   Symbol x t env 
           ->  GramTrafo env a s (Productions NF (x->a) s) (Ref (x->a) s)
newNontRF x = Trafo $ \m -> extEnv (extendMapF x m)


newtype Mapping old new 
           = Mapping (Env Ref new old) 

map2trans :: Mapping env s -> T env s
map2trans (Mapping env) 
     = T (flip lookupEnv env)


mapProd  :: T env1 env2 -> Prod NF a env1 -> Prod NF a env2
mapProd t (Sym (Nont x))      = Sym (Nont (unT t x)) 
mapProd _ (Sym (Term x))      = Sym  (Term x) 
mapProd _ (Sym TermInt)       = Sym TermInt 
mapProd _ (Sym TermChar)      = Sym TermChar 
mapProd _ (Sym TermVarid)     = Sym TermVarid 
mapProd _ (Sym TermConid)     = Sym TermConid 
mapProd _ (Sym TermOp)        = Sym TermOp 
mapProd _ (Pure x)            = Pure x
mapProd t (Star r l)          = Star     (mapProd t r) (mapProd t l)
mapProd t (FlipStar r l)      = FlipStar (mapProd t r) (mapProd t l)


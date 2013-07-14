{-# LANGUAGE Arrows, ExistentialQuantification, GADTs, Rank2Types, FlexibleContexts, ScopedTypeVariables
    , EmptyDataDecls, MultiParamTypeClasses, FlexibleInstances, OverlappingInstances, FunctionalDependencies, UndecidableInstances
    , NoMonomorphismRestriction
  #-}

module Language.Grammars.Murder where

import Language.AbstractSyntax.TTTAS
import Control.Arrow

import Language.Grammars.Grammar 
import Language.Grammars.Transformations.RemoveFix
import Language.Grammars.Transformations.RemoveEmpties
import Language.Grammars.Transformations.LeftCorner


type GramTrafo    = Trafo Unit (Productions NF)
type PreGramTrafo = Trafo Unit (Productions TL)

type ExtGram      env  start  nts   
           = PreGramTrafo env  ()   (Export start nts env)
type GramExt  env  start  nts  start' nts'  
           = PreGramTrafo env  (Export start nts env)  (Export start' nts' env)

data Export start nts env = Export (Symbol start TNonT env) (nts env)

-- add a new non-terminal to the grammar
addNT  ::  PreGramTrafo env (PreProductions TL env a) (Symbol a TNonT env)
addNT  =  proc  p -> do 
                r  <- newSRef -< prod p
                returnA -< Nont r


-- add productions to an existing non-terminal
addProds  ::  PreGramTrafo  env 
                         (Symbol a TNonT env, PreProductions TL env a) ()
addProds  = proc (nont, prds) -> do
      updateFinalEnv  -< 
         updateEnv (\ps -> PS $ (unPP prds) ++ (unPS ps)) (getRefNT nont)

-- update the productions of an existing non-terminal
updProds  ::  PreGramTrafo  env 
                         (Symbol a TNonT env, PreProductions TL env a -> PreProductions TL env a) ()
updProds  = proc (nont,f) -> do
      updateFinalEnv  -< 
         updateEnv (\ps -> PS $ (unPP . f) (PP $ unPS ps)) (getRefNT nont)


-- replace the productions of an existing non-terminal
replaceProds  ::  PreGramTrafo  env 
                         (Symbol a TNonT env, PreProductions TL env a) ()
replaceProds  = proc (nont, prds) -> do
      updateFinalEnv  -< 
         updateEnv (\_ -> PS $ (unPP prds)) (getRefNT nont)

 
-- close the grammar
closeGram :: (forall env. ExtGram  env a nts) 
                -> Grammar a 
closeGram prds  = case runTrafo prds Unit () of
     Result _ (Export (Nont r) _) gram 
            -> (leftCorner . removeEmpties) (removeFix r gram)

-- extend a grammar
extendGram  ::  (NTRecord (nts env), NTRecord (nts' env))  
            =>  ExtGram env start nts 
                -> GramExt env start nts start' nts' 
                -> ExtGram env start' nts'
extendGram g sm = g >>> sm

infixl 1 +>>

(+>>)      ::  (NTRecord (nts env), NTRecord (nts' env))  
            =>  ExtGram env start nts 
                -> GramExt env start nts start' nts' 
                -> ExtGram env start' nts'

(+>>) = extendGram


-- export
exportNTs ::  NTRecord (nts env) => PreGramTrafo env (Export start nts env) (Export start nts env)
exportNTs = returnA

-- compose grammars
{-
compG  ::  (NTUnion nts1 nts2 nts)
       =>  ExtGram env start nts1
       ->  ExtGram env start nts2
       ->  ExtGram env start nts

compG g1 g2 =  proc  () -> do
                (Export s1 ns1) <- g1 -< ()
                (Export s2 ns2) <- g2 -< ()
 
                s  <- addNT -< iI s1 Ii <|> iI s2 Ii

                returnA -< Export s (ntUnion ns1 ns2)
-}

-- extensible record


data  NTCons nt v l env  = NTCons (LSPair nt v TNonT env) (l env)
data  NTNil         env  = NTNil

class NTRecord r 
instance NTRecord (NTNil env)
instance (NTRecord (l env), NotDuplicated nt (l env)) => NTRecord (NTCons nt v l env)


class Fail err

data Duplicated nt

class NotDuplicated nt r
instance NotDuplicated nt (NTNil env)
instance Fail (Duplicated nt)      => NotDuplicated nt  (NTCons nt v l env) -- using overlapping
instance NotDuplicated nt1 (l env) => NotDuplicated nt1 (NTCons nt2 v l env)

ntNil :: NTNil env
ntNil = NTNil



infixr 4 ^| 
(^|) :: NTRecord (NTCons nt a l env) => LSPair nt a TNonT env -> l env -> NTCons nt a l env
(^|) = NTCons

{-
class GetNT nt r v | nt r -> v where
  getNT :: nt -> r -> v

data NotFound nt

instance Fail (NotFound nt) => GetNT nt (NTNil env) r where
 getNT = undefined
instance GetNT nt  (NTCons nt v l env) (Symbol v TNonT env) where -- using overlapping
 getNT _     (NTCons f _)    = symLSPair f
instance GetNT nt1 (l env) r => GetNT nt1 (NTCons nt2 v l env) r where
 getNT nont  (NTCons _ l)    = getNT nont l

instance GetNT nt (nts env) r => GetNT nt (Export start nts env) r where
 getNT nont  (Export _ nts)  = getNT nont nts
-}

class TypeEq x y b | x y -> b
instance TypeEq x x HTrue
instance HFalse ~ b => TypeEq x y b

data HTrue
data HFalse

typeEq :: TypeEq x y b => x -> y -> b
typeEq _ _ = undefined



class GetNT nt r v | nt r -> v where
  getNT :: nt -> r -> v

instance GetNTLabel nt (nts env) (Symbol a TNonT env) (nts env) => GetNT nt (Export start nts env) (Symbol a TNonT env) where
 getNT nont  (Export _ nts)  = getNTLabel (undefined :: nts env) nont nts


class GetNTLabel nt r v tenv | nt r -> v where
  getNTLabel :: tenv -> nt -> r -> v

class GetNTBool b nt r v tenv | b nt r -> v where
  getNTBool :: b -> tenv -> nt -> r -> v


data NotFound nt tenv

instance Fail (NotFound nt (tenv env)) => GetNTLabel nt (NTNil env) r (tenv env) where
 getNTLabel = undefined

instance (TypeEq nt1 nt2 b, GetNTBool b nt1 (NTCons nt2 v l env) r (tenv env)) => GetNTLabel nt1 (NTCons nt2 v l env) r (tenv env) where
 getNTLabel t nt1  l@(NTCons nt2 _)    = getNTBool (typeEq nt1 (labelLSPair nt2)) t nt1 l


instance GetNTBool HTrue nt1  (NTCons nt1 v l env) (Symbol v TNonT env) (tenv env) where 
 getNTBool _ _ _     (NTCons f _)    = symLSPair f

instance GetNTLabel nt1 (l env) r (tenv env) => GetNTBool HFalse nt1 (NTCons nt2 v l env) r (tenv env) where
 getNTBool _ t nont  (NTCons _ l)    = getNTLabel t nont l


getStart :: Export start nts env ->  (Symbol start TNonT env)
getStart (Export start _) = start

exportList :: Symbol start TNonT env -> (NTNil env -> nts env) -> Export start nts env
exportList r l = Export r $ l ntNil

export  ::  (NTRecord (l env), NotDuplicated nt (l env))
        =>  nt -> Symbol a TNonT env 
        -> l env ->  NTCons nt a l env
export l nont = (^|) (l ^= nont) 


extendExport :: Export start t env -> (t env -> nts env) -> Export start nts env
extendExport (Export r nts) ext = Export r (ext nts)


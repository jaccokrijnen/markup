{-# LANGUAGE ExistentialQuantification, GADTs, EmptyDataDecls,
    MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Language.Grammars.Grammar where

import Language.AbstractSyntax.TTTAS
import Data.HList

import Control.Applicative


-------------------------------------------
-- GRAMMAR REPRESENTATION
-------------------------------------------

data TL
data FL a
data NF


data Grammar a  
  = forall env . Grammar  (Ref a env)  
                          (FinalEnv (Productions NF) env)


newtype Productions l a env 
  = PS {unPS :: [Prod l a env]}

data Prod l a env where
      Star      ::  Prod l (a->b) env -> Prod l  a      env 
                ->  Prod l b  env
      FlipStar  ::  Prod NF a     env -> Prod NF (a->b) env 
                ->  Prod NF b  env
      Sym       ::  Symbol a t env                         
                ->  Prod l a  env
      Pure      ::  a                                      
                ->  Prod l a  env

      Fix       ::  Productions (FL a) a env               
                ->  Prod TL     a  env
      Var       ::  Prod (FL a) a  env 

-- I tried with HOAS, but it is more restrictive
--      Fix       ::  (forall s. Prod a s -> Productions a s)  -> Prod a env 


type GramEnv = Env (Productions NF)
type PreGramEnv = Env (Productions TL)


newtype PreProductions l env a 
  = PP {unPP :: [Prod l a env]}


type Line      = Int
type Column    = Int
type Filename  = String

data Pos  =  Pos      !Line !Column 
          |  PosFile  !Line !Column Filename
  deriving (Eq)

instance Show Pos where
 show (Pos (-1) (-1)) = "Built-in"
 show (Pos l    c)    = "Line: " ++ show l ++ " Column: " ++ show c
 show (PosFile (-1) (-1) _)  = "Built-in"
 show (PosFile l    c    f)  = "Line: " ++ show l ++ " Column: " ++ show c ++ " File: " ++ f
 
data DTerm a = DTerm {pos :: Pos, value :: a}
  deriving (Show, Eq)

mkDTerm :: a -> DTerm a
mkDTerm v = DTerm (Pos 0 0) v

data TTerm
data TNonT
data TAttT

data Symbol a t env where
  Term    :: String    ->        Symbol  (DTerm String)    TTerm   env

  Nont    :: Ref a env ->        Symbol  a                 TNonT   env
  -- attributed terminals
  TermInt   ::                   Symbol  (DTerm Int)       TAttT   env
  TermChar  ::                   Symbol  (DTerm Char)      TAttT   env
  TermVarid ::                   Symbol  (DTerm String)    TAttT   env
  TermConid ::                   Symbol  (DTerm String)    TAttT   env
  TermOp    ::                   Symbol  (DTerm String)    TAttT   env 
  TermSat   :: (Char -> Bool) -> Symbol  (DTerm Char)      TAttT   env
  --- TODO: the rest of EnumValToken


getRefNT :: Symbol a TNonT env -> Ref a env
getRefNT (Nont ref) = ref


pairEq :: Maybe (Equal a b) -> Maybe (Equal (a,t) (b,t))
pairEq (Just Eq) = Just Eq
pairEq Nothing   = Nothing

matchSym  ::  Symbol a t1 env -> Symbol b t2 env 
          ->  Maybe (Equal (a,t1) (b,t2))
matchSym (Nont x)   (Nont y)             = pairEq $ match x y
matchSym (Term x)   (Term y) | x == y    = Just Eq
matchSym TermInt    TermInt              = Just Eq
matchSym TermVarid  TermVarid            = Just Eq
matchSym TermConid  TermConid            = Just Eq
matchSym TermOp     TermOp               = Just Eq
matchSym TermSat    TermSat              = Just Eq
matchSym _          _                    = Nothing


int   ::  Symbol (DTerm Int)     TAttT  env
char  ::  Symbol (DTerm Char)    TAttT  env
var   ::  Symbol (DTerm String)  TAttT  env
con   ::  Symbol (DTerm String)  TAttT  env
op    ::  Symbol (DTerm String)  TAttT  env

int   =  TermInt
char  =  TermChar
var   =  TermVarid
con   =  TermConid
op    =  TermOp
sat   =  TermSat

------------------------
-- APPLICATIVE INTERFACE


sym  ::  Symbol a  t  env -> PreProductions l env a
sym  s = PP [ Sym $ s ]

nt :: Symbol a  TNonT  env -> PreProductions l env a
nt s = sym s

ntPrd :: Symbol a  TNonT  env -> PreProductions l env a
ntPrd s =  id <$> nt s
 

tr ::  String -> PreProductions l env (DTerm String)
tr s = PP [ Sym $ Term s ]


prod :: PreProductions l env a -> Productions l a env
prod (PP ps) = PS ps

varPrd   ::  PreProductions (FL a) env a
varPrd    = PP [ Var ]
fixPrd  ::  PreProductions (FL a) env a -> PreProductions TL env a
fixPrd p  = PP [ (Fix . prod) p ]


instance Functor (PreProductions l env) where
 fmap f (PP p) = PP [ Star (Pure f) p' | p' <- p ]

instance Applicative (PreProductions l env) where
 pure f = PP [ Pure f ]
 
 (PP f) <*> (PP g) = PP [ Star f' g' | f' <- f, g' <- g ]

instance Alternative (PreProductions l env) where
 empty = PP []
 
 (PP f) <|> (PP g) = PP (f ++ g)
{-
 some p = fixPrd (one <|> more)
     where  one  = (:[]) <$> toFL p 
            more = (:)   <$> toFL p <*> varPrd


 many p = fixPrd (none <|> more) 
     where  none = pure [] 
            more = (:)   <$> toFL p <*> varPrd

toFL :: PreProductions l env a -> PreProductions (FL b) env a
toFL (PP p) = PP $ map prodToFL p

prodToFL :: Prod l a env -> Prod (FL b) a env
prodToFL (Star      f  g)  = Star      (prodToFL f) (prodToFL g)
prodToFL (FlipStar  f  g)  = FlipStar  (prodToFL f) (prodToFL g)
prodToFL (Sym       s)     = Sym s
prodToFL (Pure      a)     = Pure a
prodToFL (Fix       f)     = Fix f
prodToFL Var               = error "ERROR: Language.Grammars.Grammar:1"
-}

pSome :: PreProductions (FL [a]) env a -> PreProductions TL env [a]
pSome p = fixPrd (one <|> more)
     where  one  = (:[]) <$> p 
            more = (:)   <$> p <*> varPrd

pMany :: PreProductions (FL [a]) env a -> PreProductions TL env [a]
pMany p = fixPrd (none <|> more)
     where  none = pure [] 
            more = (:)   <$> p <*> varPrd
{-
pMaybe :: PreProductions l env a -> PreProductions l env (Maybe a)
pMaybe p = (nothing <|> just)
     where  nothing = pure Nothing 
            just    = Just   <$> p 


pEither :: PreProductions l env a -> PreProductions l env b -> PreProductions l env (Either a b)
pEither p q = (left <|> right)
     where  left  = Left  <$> p
            right = Right <$> q 
-}

opt :: PreProductions l env a -> a -> PreProductions l env a
opt p a = p <|> pure a

pMaybe :: (b, (a -> b)) -> PreProductions TL env a -> PreProductions TL env b
pMaybe (n, j) p = (nothing <|> just)
     where  nothing = pure n 
            just    = j  <$> p 


pFoldr :: (a -> b -> b, b) -> PreProductions (FL b) env a -> PreProductions TL env b
pFoldr (c, e) p = fixPrd (none <|> more)
     where  none = pure e 
            more = c   <$> p <*> varPrd

------------------------
-- IDIOMS
{- 
-- | The  'Ii' is to be pronounced as @stop@
data Ii = Ii 

-- | The function 'iI' is to be pronounced as @start@
iI ::Idiomatic  l env (a -> a) g => g
iI = idiomatic (pure id)

class Idiomatic l env f g  | g -> f l env  where
    idiomatic :: PreProductions l env f -> g

instance  Idiomatic l env x  (Ii -> PreProductions l env x) where
    idiomatic ix Ii = ix


instance  Idiomatic l env f g  => Idiomatic  l env (a -> f) (PreProductions l env a -> g) where
    idiomatic isf is = idiomatic (isf <*> is)

instance  Idiomatic l env f g  => Idiomatic  l env (a -> f) (Symbol a TNonT env -> g) where
    idiomatic isf is = idiomatic (isf <*> (sym is))

instance  Idiomatic l env f g  => Idiomatic  l env ((Record HNil -> a) -> f) (Symbol a TAttT env -> g) where
    idiomatic isf is = idiomatic (isf <*> ((\x (Record HNil) -> x) <$> (sym is :: PreProductions l env a)))


instance Idiomatic l env f g => Idiomatic l env ((a -> b) -> f)  ((a -> b) -> g) where
    idiomatic isf f = idiomatic (isf <*> (pure f :: PreProductions l env (a->b)))

instance (Idiomatic  l env f g) 
       => Idiomatic  l env f (String -> g) where
    idiomatic isf str = idiomatic (isf <* (tr str))


data Kw = Kw String

kw :: String -> Kw
kw = Kw

instance  Idiomatic l env f g  => Idiomatic  l env ((Record HNil -> DTerm String) -> f) (Kw -> g) where
    idiomatic isf (Kw is) = idiomatic (isf <*> ((\x (Record HNil) -> x) <$> (tr is)))

-}
-------------------------------------------------------------------------------


newtype  LSPair nt a t env = LSPair { symLSPair :: (Symbol a t env) }  

labelLSPair :: LSPair nt a t env -> nt
labelLSPair _ = undefined

infixr 6 ^= 
(^=) :: nt -> Symbol a t env -> LSPair nt a t env
(^=) _ = LSPair

infixr 6 <=>

class LabelSymbol t v v' | t v -> v' where 
 (<=>) :: label -> Symbol v t env -> PreProductions l env (LVPair label v')


instance LabelSymbol TAttT v (Record HNil -> v) where
 l <=> v =  (\x -> l .=. (\(Record HNil) -> x)) <$> (sym v)

instance LabelSymbol TNonT v v where
 l <=> v =  (l .=.) <$> (sym v)

instance LabelSymbol TTerm v v where -- only to have all the instances
 l <=> v =  (l .=.) <$> (sym v)


{-
-------------------------------------------------------------------------------
-- Show instances for the Grammars
-- Just for debugging purposes

instance Show (Grammar a) where
 show (Grammar r prods) = show r ++ "\n" ++ show prods

instance ShowEnv (Env (Productions l) env env') => Show (Env (Productions l) env env') where
 show env = showEnv 0 env

class ShowEnv a where
    showEnv :: Int -> a -> String

instance ShowEnv (Env (Productions l) env env') where
    showEnv _ (Empty)         = "\n"
    showEnv n (Ext nts nont)  = show n ++ "->" ++ show nont ++ "\n" ++ showEnv (n+1) nts
 
instance Show (Productions l a env) where
 show (PS prods) = show prods

instance Show (Prod l a env) where
 show (Star pf pa)      = "(" ++ show pf ++ "<*>" ++ show pa ++ ")"
 show (FlipStar pa pf)  = "(" ++ show pa ++ "<**>" ++ show pf  ++ ")"
 show (Sym s)           = show s 
 show (Pure _)          = "pure"
 show (Fix f)           = "fix " ++ show f
 show Var               = "var"

instance Show (Symbol a t env) where
 show (Term s)    = show s
 show (Nont r)    = show r
 show (TermInt)   = "int"
 show (TermChar)  = "char"
 show (TermVarid) = "var"
 show (TermConid) = "con"
 show (TermOp)    = "op"

instance Show (Ref a env) where
 show Zero     = "0"
 show (Suc r)  = show  $ (1::Int) + ((read . show) r)
-}

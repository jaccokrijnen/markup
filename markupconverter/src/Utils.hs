{-# LANGUAGE Arrows, RecursiveDo, TemplateHaskell, EmptyDataDecls, NoMonomorphismRestriction, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, 
             PostfixOperators, ScopedTypeVariables, DataKinds, KindSignatures, FunctionalDependencies, UndecidableInstances #-}


module Utils where

import           Prelude hiding ((+), (*))
import           Control.Applicative
import           Control.Monad
import qualified GHC.TypeLits

import           Language.Grammars.Grammar
import           Language.Grammars.Murder
import           Language.Grammars.Murder.Derive
import           Language.Grammars.Murder.UUParsing
import qualified Text.ParserCombinators.UU.Core as UU

import Document

buildConverter :: (Document -> a) -> Parser Document -> (String -> ParseResult a)
buildConverter sem p = parse (fmap sem p)


result :: ParseResult a -> a
result (Ok x) = x
result (Rep x _) = x

errors (Ok _) = []
errors (Rep _ es) = es


parsePretty :: (Show a) => Parser a -> String -> IO a
parsePretty p input = 
    do print $ "parsing: " ++ input
       let res = parse p input
       case res of
           (Ok a)         -> return a
           (Rep a errors) -> let errors' = map (shorten 60) errors
                             in  forM_ errors' putStrLn >> putStrLn "" >> return a

shorten x e | length (show e) > x = take (x-3) (show e) ++ "..."
            | otherwise           = show e


-----
-- ? + * combinators
-----

{- 
-- (*) x = pMany $ iI x Ii :: PreProductions l [a] env
-- (+) x = pSome $ iI x iI :: PreProductions l [a] env
-- (?) x = iI Just x Ii <|> pure Nothing :: PreProductions l (Maybe a) env
-}


-- A transformation of a preproduction
newtype PreProductionsTrafo l env a b = PPT { unppt :: PreProductions l env a -> PreProductions l env b}

instance Idiomatic l env b g => Idiomatic l env a (PreProductionsTrafo l env a b -> g) where
    idiomatic prods f = idiomatic ((unppt f) prods)


-- a, b: PreProductions is a wrapper for: PreProductions l env a -> PreProductions l env b
-- x : type for a symbol in the production 
class Shortcuts l env a a' a'' b x| x -> l env a a' a'' b where
    (?) :: x -> PreProductionsTrafo l env a   b
    (+) :: x -> PreProductionsTrafo l env a'  b
    (*) :: x -> PreProductionsTrafo l env a'' b

instance Shortcuts TL env a a a a [Char] where
    (?) x = PPT (\prods -> prods <* pMaybe (Nothing, Just) (tr x))
    (+) x = PPT (\prods -> prods <* pMany (tr x))
    (*) x = PPT (\prods -> prods <* pSome (tr x))

instance Shortcuts TL env (Maybe a -> b) ([a] -> b) ([a] -> b) b (Symbol a t env) where
    (?) x = PPT (\prods -> prods <*> pMaybe (Nothing, Just) (sym x))
    (+) x = PPT (\prods -> prods <*> pSome (sym x))
    (*) x = PPT (\prods -> prods <*> pMany (sym x))

-- ignore the value of a symbol for the computation (idiomatic for <*)
data Ign a = Ign a
ign = Ign

{- Tried more general approach, not working

instance (Idiomatic l env f g, Idiomatic l env (a -> f) (x     -> g)) 
                            => Idiomatic l env (a -> f) (Ign x -> g) where
    idiomatic pprods (Ign symb) = idiomatic (pprods <*> (iI symb Ii :: PreProductions l env a))

-}

-- ignore PreProductions
instance Idiomatic l env f g => Idiomatic l env f (Ign (PreProductions l env a) -> g) where
    idiomatic prods (Ign prods') = idiomatic (prods <* prods')

-- ignore Non terminals
instance Idiomatic l env f g => Idiomatic l env f (Ign (Symbol a TNonT env) -> g) where
    idiomatic prods (Ign s) = idiomatic (prods <* sym s)

instance Idiomatic l env f g => Idiomatic l env f (Ign String -> g) where
    idiomatic prods (Ign str) = idiomatic (prods <* tr str)




-- bit of error reporting
class Error (e :: GHC.TypeLits.Symbol)

instance Error ("Missing a closing Ii") => Idiomatic l env x (PreProductions l a env) where
    idiomatic = undefined




-- also ignore PreProductions l env () and Symbol () t env
-- instance  Idiomatic l env f g  => Idiomatic  l env f (PreProductions l env () -> g) where
--    idiomatic isf is = idiomatic (isf <* is)

-- instance  Idiomatic l env f g  => Idiomatic  l env f (Symbol () t env -> g) where
--    idiomatic isf is = idiomatic (isf <* (sym is))
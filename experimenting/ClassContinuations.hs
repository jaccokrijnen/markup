{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverlappingInstances, FunctionalDependencies,
             InstanceSigs, EmptyDataDecls, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}

module ClassContinuations where

import Data.Char


-- attempt 1, a continuation that concatenates strings from showable types or strings
-- all types (String -> String ... -> String) are instance of Conc,
-- since Conc String and Conc k => Conc (String -> k)
-- therefore, all functions conc :: (String -> ... -> String) are defined
class Conc rest where
    conc :: String -> rest


-- base case
instance Conc String where
    
    conc :: String -> String
    conc = id


-- specialized inductive case
instance (Conc k) => Conc (String -> k) where
    
    conc :: String -> String -> k
    conc xs ys = conc (xs ++ ys)


-- more general inductive case
instance (Show a, Conc k) => Conc (a -> k) where
    
    conc :: String -> a -> k
    conc xs x = conc (xs ++ show x)



test :: String
test = conc "starting "     -- conc is instantiated to type
             True           -- String -> (Bool -> (Int -> String)))
             (3 :: Int)     --           |------------------------| \
                            --                    |---------------|  ) are instance of Conc
                            --                            |-------| /
                            -- =>  conc ("starting" ++ show True)         (3::Int)  :: String
                            -- =>  conc ("starting" ++ show True) ++ show (3::Int)  :: String
                            -- =>  id   ("starting" ++ show True) ++ show (3::Int)  :: String






-- attempt 2 (failed), a continuation that builds a nested tuple

class Tuple t
instance Tuple ()
instance Tuple t => Tuple (a,t)


{-
class Nest a where
    nest :: (Tuple t) => t -> a


-- base case
instance (Tuple t) => Nest t where
    nest = id                                -- problem: nest :: (Tuple t0, Tuple t1) => t0 -> t1 ???

-- inductive case
instance (Nest a)  => Nest (x -> a) where
    nest t x = (x,t)
-}




-- attempt 3 (failed), nested tuples

{-

class Nest a where
    nest :: a


instance Tuple t           => Nest (t -> t)         where
    nest = id

instance (Tuple t, Nest a) => Nest (t -> x -> a)    where
    nest t x = nest (x, t)
-}





-- atempt 4, parsers
data Parser a = Parser { getParser :: (String -> [(a, String)]) }


pSat f = let parse (x:xs) | f x = [(x,xs)] 
             parse _            = []
         in  Parser parse

infixl 5 <|>
infixl 6 <*>, <*

p <*> q = let parse xs = [(f a, xs'') | (f, xs')  <- getParser p xs, 
                                        (a, xs'') <- getParser q xs']
                            in  Parser parse
p <* q  = pure const <*> p <*> q
p <|> q = let parse xs = getParser p xs ++ getParser q xs
          in  Parser parse

pure a = let parse xs = [(a, xs)]
         in Parser parse

empty = pure ()

pMany p =  pure (:) <*> p <*> pMany p <|> pure []

pSym c = pSat (== c)
pTrm t = foldr (\c p -> pure (:) <*> pSym c <*> p) (pure []) t

pInt   = pure read <*> pMany (pSat isDigit) :: Parser Int


parseRaw :: Parser a -> String -> [(a, String)]
parseRaw (Parser p) str = p str

parse :: Parser a -> String -> Maybe (a, String)
parse (Parser p) str = case (p str) of
                            []    -> Nothing
                            (x:_) -> Just x




-- have to smuggle in the parser type a
class BuildParser a k | k -> a where
    buildp :: Parser a -> k

-- so they are equal here
-- base case
instance BuildParser a (Parser a) where
    buildp = id


-- inductive case
instance BuildParser b k => BuildParser (a -> b) (Parser a -> k) where
    buildp :: Parser (a -> b) -> Parser a -> k
    buildp p q = buildp (p <*> q)

data Brackets = Pair Brackets Brackets | Empty
instance Show Brackets where
    show Empty    = ""
    show (Pair l r) = "(" ++ show l ++ ")" ++ show r


-- example of notation
pBrackets :: Parser Brackets
pBrackets =  buildp      (pure semPair)    ( pSym '(' )    pBrackets    (pSym ')')    pBrackets 
         <|> pure Empty
    where semPair :: Char -> Brackets -> Char -> Brackets -> Brackets
          semPair _ x _ y = Pair x y





-- more awesome specialized cases

-- Ignore a character
instance BuildParser b k => BuildParser b (Char -> k) where
    buildp p c = buildp (p <* (pSym c))

instance BuildParser b k => BuildParser b (String -> k) where
    buildp p x = buildp (p <* (pTrm x))

-- Lift a function in pure
instance BuildParser b k => BuildParser ((x -> y) -> b) ((x -> y) -> k) where
    buildp p f = buildp p (pure f)

rhs :: BuildParser (a -> a) k => k
rhs = buildp (pure id)


-- example of better notation
s :: Parser Brackets
s =       rhs   semPair  "(" s ")" s
      <|> rhs   semEmpty "" 

      where semPair  = Pair
            semEmpty = const Empty 

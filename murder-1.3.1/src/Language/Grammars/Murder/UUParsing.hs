module Language.Grammars.Murder.UUParsing where


import qualified Text.ParserCombinators.UU.Core as UU
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances hiding (Parser,input,msgs)

import qualified Data.Set as Set
import Data.List (nub)

import Language.Grammars.Grammar
import Language.AbstractSyntax.TTTAS


type Parser a = P (Str Char String LineCol) a

pInt :: Parser Int
pInt  = pNatural -- pInteger
pChr :: Parser Char
pChr  = pAscii

pVar' :: Parser String
pVar'  = ((:) <$> pLower <*> pList pIdChar `micro` 2) 

pVar :: Set.Set String -> Parser String
pVar kws =  (addLength 0 (pVar' >>= \i -> if i `Set.member` kws then pToken ("_not_reserved") else return i)) <* pSpaces

pCon' :: Parser String
pCon'  = ((:) <$> pUpper <*> pList pIdChar `micro` 2)

pCon :: Set.Set String -> Parser String
pCon kws = (addLength 0 (pCon' >>= \i -> if i `Set.member` kws then pToken ("_not_reserved") else return i))  <* pSpaces 

pIdChar :: Parser Char
pIdChar = pLower <|> pUpper <|> pDigit <|> pAnySym "='"

pOp :: Parser String
pOp   = (pList1 $ pAnySym ('|':"!#$%&*+./<=>?@\\^-~:") `micro` 2) <* pSpaces

pTerm :: String -> Parser String
pTerm keyw = pToken keyw `micro` 1 <* pSpaces

pSpaces' :: Parser String
pSpaces' = (:) <$> pAnySym " \r\n\t" <*> pSpaces

lc2Pos :: LineCol -> Pos
lc2Pos (LineCol l c) = Pos l (c+1)

pSat :: [Char] -> Parser Char
pSat = pAnySym 
      
newtype Const f a s = C {unC :: f a}

-- | The function 'compile' generates a parser out of a closed grammar 
compile :: Grammar a -> Parser a
compile = compileKws Set.empty

-- | The function 'compileKws' generates a parser out of a closed grammar, restricting the identifiers to not belong to the list of reserved words
compileKws :: Set.Set String -> Grammar a -> Parser a
compileKws kws (Grammar (start :: Ref a env) rules) 
                       = id <$ pSpaces <*> (unC (lookupEnv start result))
  where  result  =  
          mapEnv 
          (\ (PS ps) -> C (foldr1 (<|>) [ comp p | p <- ps]))
          rules

         comp :: forall t . Prod NF t env -> Parser t

         comp (Star     x y)     = comp x <*>   comp y
         comp (FlipStar x y)     = comp x <**>  comp y
         comp (Pure     x)       = pure x

         comp (Sym (Term t))     = (DTerm . lc2Pos) <$> pPos <*> pTerm t     <?> t
         comp (Sym (Nont n))     = unC (lookupEnv n result)

         comp (Sym TermInt)      = (DTerm . lc2Pos) <$> pPos <*> pInt        <?> "number"
         comp (Sym TermChar)     = (DTerm . lc2Pos) <$> pPos <*> pChr        <?> "character"
         comp (Sym TermVarid)    = (DTerm . lc2Pos) <$> pPos <*> (pVar kws)  <?> "identifier"
         comp (Sym TermConid)    = (DTerm . lc2Pos) <$> pPos <*> (pCon kws)  <?> "constructor"
         comp (Sym TermOp)       = (DTerm . lc2Pos) <$> pPos <*> pOp         <?> "operator"
         comp (Sym (TermAnyOf x)) = (DTerm . lc2Pos) <$> pPos <*> pSat x     <?> "any of: " ++ x

mapEnv  ::  (forall a . f a s -> g a s)  
        ->  Env f s env -> Env g s env
mapEnv  _ Empty       = Empty
mapEnv  f (Ext r v)   = Ext (mapEnv f r) (f v)

generate = compileKws

data ParseResult a = Ok  a
                   | Rep a [Error LineCol] 
      deriving Show

nuberror :: Error a -> Error a
nuberror (Inserted m p ms) = Inserted m p (nub ms)
nuberror (Deleted  m p ms) = Deleted  m p (nub ms)	
nuberror (Replaced m1 m2 p ms) = Replaced m1 m2 p (nub ms) 	
nuberror (DeletedAtEnd s)  = (DeletedAtEnd s)	

-- | The function 'parse' runs the parser for an input.
parse :: Parser a -> String -> ParseResult a
parse p input = case UU.parse ( (,) <$> p <*> pEnd) (createStr (LineCol 1 1) input) of
                  (a,[]  ) -> Ok a
                  (a,msgs) -> Rep a $ map nuberror msgs



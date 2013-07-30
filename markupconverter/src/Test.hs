{-# LANGUAGE Arrows, DoRec, TemplateHaskell, EmptyDataDecls, NoMonomorphismRestriction, FlexibleInstances, MultiParamTypeClasses, PostfixOperators #-}

module Test (
    module Language.Grammars.Murder.UUParsing,
    primitives,
    parser)

    where

import Control.Applicative

import Prelude hiding ((+), (*))
import Language.Grammars.Grammar
import Language.Grammars.Murder
import Language.Grammars.Murder.Derive
import Language.Grammars.Murder.UUParsing

import Document

$(csLabels  ["cs_Newline", "cs_Line", "cs_Header"])

$(csLabels ["cs_Root"])



-- | An extensible grammar with common lexical structures for markup languages
primitives = proc () -> do
    
    rec 
        newline <- addNT -< iI semNewLine ("\r"?) "\n" Ii
        
        line    <- addNT -< iI semLine (pMany $ sym (anyexcept "\r\n")) Ii <* nt newline 
        
        header  <- addNT -< iI semHeader  ("#"+) line Ii
        
    
    exportNTs -< exportList header ( export cs_Header  header
                                   . export cs_Newline newline 
                                   . export cs_Line    line)



semLine = map (\(DTerm _ x) -> x)

semNewLine :: Maybe (DTerm String) -> ()
semNewLine _ = ()

semHeader :: [DTerm String] -> String -> Block
semHeader hs str = Header (length hs) (Plain str)





parser = compile (closeGram primitives)



testGram = proc () -> do
    rec 
        root <- addNT -< length <$> pSome (tr "#") <* pMany (sym $ anyexcept "abc")
    
    exportNTs -< exportList root (export cs_Root root)

test = compile (closeGram testGram)

-- sem = iI 





-- x : type for a symbol in the production
-- l : the symbol level (toplevel/fixpointlevel/no fixpoint)
-- a : The type of value that the symbol represents
class Shortcuts x l a where
    (?) :: x -> PreProductions l env (Maybe a)
    (+) :: x -> PreProductions l env [a]
    (*) :: x -> PreProductions l env [a]

instance Shortcuts [Char] TL (DTerm String) where
    (?) x = iI Just (tr x) Ii <|> pure Nothing
    (+)   = pSome . tr
    (*)   = pMany . tr
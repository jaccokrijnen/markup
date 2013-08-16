{-# LANGUAGE Arrows, RecursiveDo, EmptyDataDecls, TemplateHaskell, PostfixOperators #-}

module Grammars.Markdown (gMarkdown,
                          pMarkdown)

    where


import Prelude hiding ((+), (*))
import Control.Applicative

import Language.Grammars.Grammar
import Language.Grammars.Murder
import Language.Grammars.Murder.Derive
import Language.Grammars.Murder.UUParsing



import Decl.Document
import Utils

$(csLabels  ["cs_Newline", "cs_Line", "cs_Header"])



-- Markdown grammar
gMarkdown = proc () -> do
    
    rec 
        newline <- addNT -< iI semNewLine         (tr "\r" <|> tr "\n") Ii
        
        line    <- addNT -< iI semLine            (pMany . sym . anyexcept $ "\r\n") (ign newline) Ii
        
        header  <- addNT -< iI semHeaderAtx       (pSome (tr "#")) line Ii
                       --  <|> iI semHeaderSetext    line  
                       --                          (tr "=" <|> tr "-") (ign line) Ii 
        
    
    exportNTs -< exportList header ( export cs_Header  header
                                   . export cs_Newline newline 
                                   . export cs_Line    line)



lineChars = ['a'..'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " ':;!@()\""


-- Semantics for building the AST
semLine = map value

--semNewLine :: Maybe (DTerm String) -> ()
semNewLine _ = ()

semHeaderAtx :: [DTerm String] -> String -> Block
semHeaderAtx x str = let hlevel = length x
                     in  Header hlevel [Plain str]

semHeaderSetext :: String -> (DTerm String) -> Block
semHeaderSetext str hsym = let hlevel = if (value hsym) == "=" then 1 else 2
                           in  Header hlevel [Plain str]



-- Parser
pMarkdown = compile (closeGram gMarkdown)

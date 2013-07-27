module Scanner where

import Data.Char

data Token = TNewline
           | TWhitespace Int
           | TChar       Char
           



tabSize :: Int
tabSize = 4


scan :: String -> [Token]
scan []                   = []
scan xs | match wspace xs = 
        | match nlines xs = TNewline
        | otherwise       = TChar (head xs)


match strs = any (isPrefixOf xs) strs

wspace = [" ", "\t"]
nlines = ["\n", "\r\n"]
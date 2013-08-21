module HTML2HTML where

-- import Utils (buildConverter, result, errors)

import Language.Grammars.Murder.UUParsing

import Grammars.HTML
import Semantics.HTML

{- 
html2html = buildConverter semHTMLNotExtensible pHTML


example = 
	do input <- readFile "examples/inputLarge.html"
	   let output = html2html input
	   
	   mapM_ print (errors output)
	   writeFile "examples/outputLarge.html" (result output) -}
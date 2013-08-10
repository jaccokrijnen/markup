module HTML2HTML where

import Utils (buildConverter, result, errors)

import Language.Grammars.Murder.UUParsing

import Grammars.HTML
import Semantics.HTML


html2html = buildConverter semHTML pHTML


example = 
	do input <- readFile "examples/input.html"
	   let output = html2html input
	   
	   mapM_ print (errors output)
	   writeFile "examples/output2.html" (result output)
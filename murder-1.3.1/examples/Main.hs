import qualified Data.Set as Set

import Control.Arrow

import UU.Pretty

import LangDef
import LangSem

import LangExt

import Language.Grammars.Murder
import Language.Grammars.Murder.UUParsing

import Language.Grammars.AspectAG


gram = closeGram prds
test1 = lang1 "prog.src"
lang1 s = do
--        tokens <- scanFile gramOpts "prog.src"
        tokens <- readFile s

        let r = (parse . compile ) gram tokens

        case ( parse . compile ) gram tokens of
           (Ok fres) -> do 
                          let res = fres emptyRecord
                          print $ res # spp
                          print $ res # sval
           (Rep _ err) -> print err




gram' = closeGram (prds +>> prds') 
test2 = lang2 "prog2.src"
lang2 s = do
--        tokens <- scanFile (gramOpts' gramOpts) "prog2.src"
        tokens <- readFile s
        case ( parse . compile ) gram' tokens of
           (Ok fres) -> do 
                          let res = fres emptyRecord
                          print $ res # spp
                          print $ res # sval
           (Rep _ err) -> print err

gram'' = closeGram (prds +>> prds' +>> prds'') 
test3 = lang3 "prog3.src"
lang3 s = do
--        tokens <- scanFile ((gramOpts'' . gramOpts') gramOpts) "prog3.src"
        tokens <- readFile s
        case ( parse . compile ) gram'' tokens of
           (Ok fres) -> do 
                          let res = fres emptyRecord
                          print $ res # spp
                          print $ res # sval
           (Rep _ err) -> print err


main = test1 >> test2 >> test3



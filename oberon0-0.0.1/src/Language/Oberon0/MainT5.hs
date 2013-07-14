{-# OPTIONS -fcontext-stack=1000 #-}

module Language.Oberon0.MainT5 (module Language.Oberon0.MainT5, module Language.Oberon0.MainT2) where

import System.Console.GetOpt
import Control.Monad (when)
import Language.C

import Language.Oberon0.MainT2 hiding (options)
    
options :: [OptDescr Flag]
options =
     [ Option ['p']     ["pretty"]  (OptArg PP               "FILE")   "pretty-print"
     , Option ['g']     ["gen"]     (OptArg Gen              "FILE")   "generate C code"
     , Option ['e']     ["errors"]  (ReqArg (MaxErr . read)  "NUMBER") "maximum number of errors to list"
     ]
    

gencode     ((Gen mbfile):_)  cgen = let ccode = "#include <stdio.h>\n" ++ show (pretty cgen)
                                     in  case mbfile of
                                            Just file -> writeFile file ccode
                                            Nothing   -> do putStrLn "Generated code:"
                                                            putStrLn ccode
gencode     (_:fs)            cgen = gencode fs cgen
gencode     []                _    = return ()

 
mainT5 ppatt erratt cgenatt = mainTemplate options $ \flags atts -> do  
                                                         prettyprint flags (ppatt atts)
                                                         errors <- printerrors flags (erratt atts) 
                                                         when (not errors) $  gencode flags (cgenatt atts)


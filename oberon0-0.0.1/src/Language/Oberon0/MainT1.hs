{-# OPTIONS -fcontext-stack=1000 #-}

module Language.Oberon0.MainT1 where

import System.Environment( getArgs )
import System.Console.GetOpt

import Language.Grammars.Murder
import Language.Grammars.Murder.UUParsing



data Flag  = MaxErr Int | PP (Maybe String) | Gen (Maybe String) 
       deriving (Show, Eq, Ord)
    
options :: [OptDescr Flag]
options =
     [ Option ['p']     ["pretty"]  (OptArg PP               "FILE")   "pretty-print"
     , Option ['e']     ["errors"]  (ReqArg (MaxErr . read)  "NUMBER") "maximum number of errors to list"
     ]
    

maxerrors   ((MaxErr e):_)  le = if e < le then e else le
maxerrors   (_:fs)          le = maxerrors fs le
maxerrors   []              le = le

prettyprint ((PP mbfile):_ )  ppcode = case mbfile of
                                            Just file -> writeFile file (show ppcode)
                                            Nothing   -> do putStrLn "Pretty-printed code:"
                                                            print ppcode
prettyprint (_:fs)            ppcode = prettyprint fs ppcode
prettyprint []                _      = return ()



remComments ('(':'*':xs) level = ' ':' ':(remComments xs (level + 1))
remComments ('*':')':xs) level | level > 0 = ' ':' ':(remComments xs (level - 1))
                               | otherwise =  '*':')':xs
remComments ('\n':xs) level    = '\n':(remComments xs level)

remComments (x:xs) level | level > 0 = ' ':(remComments xs level)
                         | otherwise = x:(remComments xs level)
remComments [] _ = []


mainTemplate opts task ip prg kws gram = do
	args <- getArgs
        case getOpt Permute opts args of
          (flags, prg:nonOpts, []) -> do  tokens <- readFile prg
                                          case (parse . compileKws kws) gram (remComments tokens 0) of
           					(Ok res)    -> do  let atts = res ip 
                                                                   task flags atts
                                                                        
                                                (Rep _ err) -> do  let maxerr  = maxerrors flags (length err)
                                                                   putStrLn "Errors found while parsing:"
                                                                   mapM_ (putStrLn . show) $ take maxerr err
                                                                   putStrLn $ "--- Listed " ++ show maxerr ++ " of " ++ show (length err)

          (_, _, errs) -> ioError (userError (concat errs ++ usageInfo ("Usage: " ++ prg ++ " [OPTIONS] file") opts)) 



mainT1 ppatt = mainTemplate options $ \flags atts -> prettyprint flags (ppatt atts)


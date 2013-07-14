{-# OPTIONS -fcontext-stack=1000 #-}

module Language.Oberon0.MainT2 (module Language.Oberon0.MainT2, module Language.Oberon0.MainT1) where

import Language.Oberon0.MainT1

printerrors flags errs = do  let maxerr = maxerrors flags (length errs)
                             if maxerr == 0
                              then do return False
                              else do putStrLn "Errors found while compiling:"
                                      mapM_ putStrLn $ take maxerr errs
                                      putStrLn $ "--- Listed " ++ show maxerr ++ " of " ++ show (length errs)
                                      return True


mainT2 ppatt erratt = mainTemplate options $ \flags atts -> do  
                                                         prettyprint flags (ppatt atts)
                                                         printerrors flags (erratt atts)
                                                         return () 


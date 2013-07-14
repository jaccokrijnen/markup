{-# LANGUAGE TemplateHaskell, EmptyDataDecls #-}

module Language.Grammars.Murder.Derive (csLabel, csLabels) where

import Language.Haskell.TH
import Control.Monad

declareLabel :: Name -> Name -> TypeQ -> Q [Dec]
declareLabel ndata nlabel t = do 
            dtl <- dataD (cxt []) ndata [] [] []
            lbl <- declareFnLabel nlabel t
            return $ dtl:lbl

declareFnLabel ::  Name -> TypeQ -> Q [Dec]
declareFnLabel nlabel t = do 
            sgn <- sigD nlabel t  
            let pxy = normalB [| undefined |]
            lbl <- funD nlabel [clause [] pxy []]
            return [sgn,lbl]

csLabel ::  String -> Q [Dec]
csLabel nt = declareLabel ntTn ntn  (conT $ ntTn) 
  where
      ntn   = mkName nt
      ntTn  = mkName $ "Cs_" ++ nt

csLabels ::  [String] -> Q [Dec]
csLabels = liftM concat . mapM csLabel


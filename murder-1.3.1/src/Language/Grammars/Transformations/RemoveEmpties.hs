
module Language.Grammars.Transformations.RemoveEmpties (removeEmpties) where


import Language.Grammars.Grammar
import Language.AbstractSyntax.TTTAS


data HasEmpty a env = Unknown | HasEmpty (Prod NF a env) | HasNotEmpty
--  deriving (Show)


removeEmpties :: Grammar a -> Grammar a
removeEmpties (Grammar start prods) 
     =  Grammar start $ removeEmptiesEnv (findEmpties prods) prods 



findEmpties :: GramEnv env env -> Env HasEmpty env env 
findEmpties prods = findEmpties' prods (initEmpties prods)
         where
            findEmpties' prds empties = 
                  case stepFindEmpties empties prds empties of
                      (empties', True,  _)     -> findEmpties' prds empties'
                      (empties', False, False) -> empties'
                      (_,        False, True)  -> error "Remove Empties(1): Incorrect Grammar!!"

initEmpties :: GramEnv use def -> Env HasEmpty use def 
initEmpties Empty        = Empty
initEmpties (Ext nts _)  = Ext (initEmpties nts) Unknown

stepFindEmpties :: Env HasEmpty use use -> GramEnv use def -> Env HasEmpty use def 
                -> (Env HasEmpty use def, Bool, Bool) 
stepFindEmpties _       Empty          Empty        
    = (Empty, False, False)
stepFindEmpties empties (Ext rprd prd) (Ext re e)   
    = let (re',rchanged,rhasUnk) = stepFindEmpties empties rprd re
          (e', changed, hasUnk)  = updateEmpty empties prd e
      in  (Ext re' e', changed || rchanged, hasUnk || rhasUnk)
stepFindEmpties _       _              _ 
    = error "RemoveEmpties(2): Error in the transformation!!!"        


updateEmpty :: Env HasEmpty use use -> Productions NF a use -> HasEmpty a use  
            -> (HasEmpty a use, Bool, Bool) 
updateEmpty _       _   (HasEmpty p) = (HasEmpty p,  False, False)
updateEmpty _       _   HasNotEmpty  = (HasNotEmpty, False, False)
updateEmpty empties prd Unknown      = case hasEmpty empties prd of
                                        Unknown -> (Unknown, False, True)
                                        e       -> (e,       True,  False)

hasEmpty :: Env HasEmpty env env -> Productions NF a env -> HasEmpty a env
hasEmpty empties (PS ps)= foldr (\p re -> combHasEmpty (isEmpty p empties) re) HasNotEmpty ps


combHasEmpty :: HasEmpty a env -> HasEmpty a env -> HasEmpty a env
combHasEmpty (HasEmpty _)     (HasEmpty _)     = error "Remove Empties(3): Ambiguous Grammar!!!"
combHasEmpty _                (HasEmpty p)     = HasEmpty p
combHasEmpty (HasEmpty p)     _                = HasEmpty p
combHasEmpty HasNotEmpty      HasNotEmpty      = HasNotEmpty
combHasEmpty _                Unknown          = Unknown
combHasEmpty Unknown          _                = Unknown


isEmpty :: Prod NF a env -> Env HasEmpty env env -> HasEmpty a env
isEmpty (Pure a)              _       = HasEmpty (Pure a)
isEmpty (Sym (Term _))        _       = HasNotEmpty
isEmpty (Sym (TermInt))       _       = HasNotEmpty
isEmpty (Sym (TermChar))      _       = HasNotEmpty
isEmpty (Sym (TermVarid))     _       = HasNotEmpty
isEmpty (Sym (TermConid))     _       = HasNotEmpty
isEmpty (Sym (TermOp))        _       = HasNotEmpty
isEmpty (Sym (TermAnyOf _))   _       = HasNotEmpty
isEmpty (Sym (TermAnyExcept _)) _     = HasNotEmpty
isEmpty (Sym (Nont r))        empties = lookupEnv r empties
isEmpty (Star pl pr)          empties = case isEmpty pl empties of
                                         HasEmpty (Pure f)  -> case isEmpty pr empties of
                                                                HasEmpty (Pure x)  -> HasEmpty $ Pure (f x)
                                                                HasEmpty _         -> error "RemoveEmpties(4): Error in the transformation!!!"
                                                                HasNotEmpty        -> HasNotEmpty
                                                                Unknown            -> Unknown
                                         HasEmpty _         -> error "RemoveEmpties(5): Error in the transformation!!!"
                                         HasNotEmpty        -> HasNotEmpty
                                         Unknown            -> case isEmpty pr empties of
                                                                HasNotEmpty        -> HasNotEmpty
                                                                _                  -> Unknown
isEmpty (FlipStar pl pr)      empties = case isEmpty pl empties of
                                         HasEmpty (Pure x)  -> case isEmpty pr empties of
                                                                HasEmpty (Pure f)  -> HasEmpty $ Pure (f x)
                                                                HasEmpty _         -> error "RemoveEmpties(6): Error in the transformation!!!"
                                                                HasNotEmpty        -> HasNotEmpty
                                                                Unknown            -> Unknown
                                         HasEmpty _         -> error "RemoveEmpties(7): Error in the transformation!!!"
                                         HasNotEmpty        -> HasNotEmpty
                                         Unknown            -> case isEmpty pr empties of
                                                                HasNotEmpty        -> HasNotEmpty
                                                                _                  -> Unknown




removeEmptiesEnv :: Env HasEmpty use use -> GramEnv use def -> GramEnv use def
removeEmptiesEnv _       Empty         
    = Empty
removeEmptiesEnv empties (Ext rprds prds)    
    = Ext (removeEmptiesEnv empties rprds) (removeEmpty empties prds)


removeEmpty :: Env HasEmpty env env -> Productions NF a env -> Productions NF a env
removeEmpty empties (PS prds) = PS $ foldr ((++) . remEmptyProd) [] prds 
  where 
     -- if we don't allow the starting point to be empty, 
     -- then we can ignore the empty part
     remEmptyProd prd = let (prd',_) = splitEmpty empties prd
                        in  prd'


splitEmpty :: Env HasEmpty env env -> Prod NF a env -> ([Prod NF a env], Maybe (Prod NF a env))

splitEmpty  empties  (Star f g)  
    =  let (fne, fe) = splitEmpty empties f 
           (gne, ge) = splitEmpty empties g 

           fne_gne   = [ Star fv gv | fv <- fne, gv <- gne ] 
           fne_ge    = case ge of
                         Nothing -> []
                         Just gv -> [ Star fv gv | fv <- fne] 

           fe_gne  = case fe of
                         Nothing -> []
                         Just fv -> [ FlipStar gv fv | gv <- gne] 
           fe_ge   = do 
                         (Pure fv) <- fe
                         (Pure gv) <- ge
                         return $ Pure (fv gv)

       in  (fne_gne ++ fne_ge ++ fe_gne , fe_ge)


splitEmpty  empties  (Sym (Nont r))  
    = case lookupEnv r empties of
              HasEmpty (Pure f)  -> ([Sym $ Nont r], Just (Pure f))
              HasEmpty _         -> error "RemoveEmpties(9): Error in the transformation!!!"
              _                  -> ([Sym $ Nont r], Nothing)
--              HasNotEmpty        -> ([Sym $ Nont r], Nothing)
--              Unknown            -> error "RemoveEmpties(10): Error in the transformation!!!"

splitEmpty  _        (Sym  s)  = ([Sym s], Nothing)
splitEmpty  _        (Pure a)  = ([],Just $ Pure a)

splitEmpty  _        (FlipStar _ _)  
    = error "RemoveEmpties(11): FlipStar cannot be used to define grammars."



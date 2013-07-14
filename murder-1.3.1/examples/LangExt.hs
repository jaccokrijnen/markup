{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE  Arrows, DoRec, EmptyDataDecls, FlexibleContexts, TemplateHaskell, NoMonomorphismRestriction,
    RankNTypes #-}

module LangExt where


import Control.Arrow

import UU.Pretty

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive

import Language.Grammars.Murder
import Language.Grammars.Grammar hiding ((<=>))


import LangSem
import Utils

import Control.Applicative

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1



-- modifications of the semantics
synM = synmodM
inhM = inhmodM


--Syntax Macro 1 -------------------------------------------------------------- 

type AttExpr =  Record (HCons (LVPair (Proxy Att_ienv) [(String,Int)]) HNil)
                -> Record (HCons (LVPair (Proxy Att_spp) PP_Doc)
                          (HCons (LVPair (Proxy Att_sval) Int) HNil))




--Square
-- $(chLabel "se" ''T_Expr)
$(addProd "Sq" [ ("se",''T_Expr) ])

sppSq = synM spp $ do me1 <- at ch_se 
                      return $ "square" >#< (me1 # spp)   

se2m r = (ch_me1 .=. (r # ch_se) .*. ch_me2 .=. (r # ch_se) .*. emptyRecord)
m2se r = (ch_se .=. (r # ch_me1) .*. emptyRecord)


-- aspSq = sppSq `ext` (adapt aspMul se2m se2m m2se)
aspSq = sppSq `ext` (mapChildren aspMul (ch_me1 .=. ch_se .*. ch_me2 .=. ch_se .*. emptyRecord))

-- semSq = \s -> knit aspSq (s .*. emptyRecord)  
semSq = semP_Sq aspSq

--Pyth
-- $(chLabels ["pe1","pe2"] ''T_Expr)
$(addProd "Pyth" [ ("pe1",''T_Expr), ("pe2",''T_Expr) ])

sppSq' = synM spp $ do liftM (# spp) (at ch_se) 
aspSq' = sppSq' `ext` aspSq 

sppPyth  = synM spp  $ do  pe1 <- at ch_pe1
                           pe2 <- at ch_pe2 
                           return $ "pyth" >#< (pe1 # spp) >#< (pe2 # spp)   
{-
aspAdd' =  graft  (graft  aspAdd
                          (ch_ae2 .=. ch_ae2 .*. emptyRecord)
                          ch_ae1 
                          aspSq' 
                          (ch_se .=. ch_pe1 .*. emptyRecord))
                  (ch_pe1 .=. ch_pe1 .*. emptyRecord)
                  ch_ae2
                  aspSq' 
                  (ch_se .=. ch_pe2 .*. emptyRecord)
-}

aspAdd' =  agMacro  (aspAdd, ch_ae1 ==> (aspSq', ch_se --> ch_pe1)
                             <.> 
                             ch_ae2 ==> (aspSq', ch_se --> ch_pe2))


aspPyth    = sppPyth `ext` aspAdd'
-- semPyth = \p1 p2 -> knit  aspPyth (p1 .*. p2 .*. emptyRecord)  
semPyth = semP_Pyth aspPyth


--AddSq
$(addProd "AddSq" [ ("as1",''T_Expr), ("as2",''T_Expr) ])


sppAddSq  = synM spp  $ do  as1 <- at ch_as1
                            as2 <- at ch_as2 
                            return $ "addsq" >#< (as1 # spp) >#< (as2 # spp)   



{-
aspAddSq = sppAddSq `ext`
           (graft  (graft  aspMul
                           (ch_me2 .=. ch_me2 .*. emptyRecord)
                           ch_me1 
                           aspAdd 
                           (ch_ae1 .=. ch_as1 .*. ch_ae2 .=. ch_as2 .*. emptyRecord))
                   (ch_as1 .=. ch_as1 .*. ch_as2 .=. ch_as2 .*. emptyRecord)
                   ch_me2
                   aspAdd 
                   (ch_ae1 .=. ch_as1 .*. ch_ae2 .=. ch_as2 .*. emptyRecord))
-}

aspAddSq = sppAddSq `ext`
            agMacro  (aspMul   ,   ch_me1 ==> (aspAdd, ch_ae1 --> ch_as1 <.> ch_ae2 --> ch_as2)
                              <.>  ch_me2 ==> (aspAdd, ch_ae1 --> ch_as1 <.> ch_ae2 --> ch_as2))


semAddSq = semP_AddSq aspAddSq


--Parenthesis
-- $(chLabel "pe" ''T_Expr)
$(addProd "Par" [ ("pe",''T_Expr) ])

sppPar  = syn spp  $ do  pe <- at ch_pe
                         return $ "(" >|< pe # spp >|< ")" 
svalPar = syn sval $ do  liftM (# sval) (at ch_pe)
ienvPar = copy ienv exprNT

aspPar = sppPar `ext` ienvPar `ext` svalPar
-- semPar = \e -> knit aspPar (e .*. emptyRecord)
semPar = semP_Par aspPar


--Substitution
sppSubst = synM spp $ do  lnm   <- valAt ch_lnm
                          val   <- at ch_val
                          body  <- at ch_body
                          return $ (body # spp) >|< "[" >|< (pp lnm)  >|< " | " >|< (val # spp) >|< "]" 

semSubst = \b l v -> semP_Let (sppSubst `ext` aspLet) l v b




--Grammar Extension
{-
prds'  ::  ( NTRecord (nts env) 
           , GetNT NTExp    (nts env)  (Symbol AttExpr TNonT env)
           , GetNT NTTerm   (nts env)  (Symbol AttExpr TNonT env)
           , GetNT NTFactor (nts env)  (Symbol AttExpr TNonT env)) 
       =>    GramExt env start nts start nts
-}
prds' = proc imported -> do
         let exp     = getNT ntExp     imported
         let term    = getNT ntTerm    imported
         let factor  = getNT ntFactor  imported

         addProds   -<  (exp,     ( iI semSubst  exp "[" var "|" exp "]" Ii))

         addProds   -<  (term,    ( iI semSq     "square"  factor Ii)  <|>
                                  ( iI semPyth   "pyth"    factor factor Ii) <|>
                                  ( iI semAddSq  "addsq"   factor factor Ii))
 
         addProds   -<  (factor,  ( iI semPar    "(" exp ")" Ii )    )
         
         exportNTs  -< imported



--gramOpts' gramOpts = gramOpts `extKeywordsTxt` [ "square", "pyth" ] `extSpecChars` "()[|]"



--Syntax Macro 2 --------------------------------------------------------------

--Double
-- $(chLabel "de" ''T_Expr)
$(addProd "Db" [ ("de",''T_Expr) ])

--aspTwo = fixCst aspCst ch_cv 2
{-
aspMul' =  graft  aspMul
                  (ch_me2 .=. ch_de .*. emptyRecord) 
                  ch_me1
                  aspTwo
                  emptyRecord 
-}

aspMul' =  agMacro  (aspMul   ,   ch_me1 ==> (aspCst, ch_cv ~~> (DTerm undefined 2))
                             <.>  ch_me2 --> ch_de)
                  

sppDb = synM spp $ do de <- at ch_de 
                      return $ "double" >#< (de # spp)   

aspDb = sppDb `ext` aspMul'
                
--semDb = \d -> knit aspDb (d .*. emptyRecord)
semDb = semP_Db aspDb
           


--AddMul
-- $(chLabels ["am1","am2","am3"] ''T_Expr)
$(addProd "AddMul" [ ("am1",''T_Expr), ("am2",''T_Expr), ("am3",''T_Expr) ])


sppAddMul = synM spp $ do  am1 <- at ch_am1 
                           am2 <- at ch_am2 
                           am3 <- at ch_am3 
                           return $ "addmul" >#< (am1 # spp) >#< (am2 # spp) >#< (am3 # spp)  
{-
aspAddMul = ext sppAddMul $  graft  aspAdd
                                    (ch_ae1 .=. ch_am1 .*. emptyRecord)
                                    ch_ae2 
                                    aspMul
                                    (ch_me1 .=. ch_am2 .*. ch_me2 .=. ch_am3 .*. emptyRecord)
-}
aspAddMul = sppAddMul `ext`
            agMacro  (aspAdd   ,  ch_ae1 --> ch_am1
                              <.> ch_ae2 ==> (aspMul, ch_me1 --> ch_am2 <.> ch_me2 --> ch_am3))

 
-- semAddMul = \p1 p2 p3 -> knit aspAddMul (p1 .*. p2 .*. p3 .*. emptyRecord)
semAddMul = semP_AddMul aspAddMul                          


--Grammar Extension
{-
prds''  ::    ( NTRecord (nts env) 
              , GetNT NTTerm   (nts env)  (Symbol AttExpr TNonT env)
              , GetNT NTFactor (nts env)  (Symbol AttExpr TNonT env)) 
       =>    GramExt env start nts start nts
-}
prds'' = proc imported -> do
         let term    = getNT ntTerm    imported
         let factor  = getNT ntFactor  imported


         addProds   -<  (term,     (iI semDb      "double"  factor Ii) <|>
                                   (iI semAddMul  "addmul"  factor factor factor Ii))

         
         exportNTs  -< imported



--gramOpts'' gramOpts = gramOpts `extKeywordsTxt` [ "double", "addmul" ] `extSpecChars` "()[|]"






module Language.Grammars.Murder.UULib where

import qualified UU.Parsing as UU
import Language.Grammars.Murder.Scanner hiding (Pos)
import qualified Language.Grammars.Murder.Scanner as S

import Language.Grammars.Grammar
import Language.AbstractSyntax.TTTAS

toPos :: S.Pos -> Pos
toPos (S.Pos l c f) = PosFile l c f

toDTerm :: (a -> b) -> (a, S.Pos) -> DTerm b
toDTerm f p =  DTerm ((toPos . snd) p) ((f . fst) p)

pChr            ::   UU.Parser Token (DTerm Char)
pChr            =    (toDTerm head) UU.<$> pCharPos
pInt            ::   UU.Parser Token (DTerm Int)
pInt            =    (toDTerm read) UU.<$> pIntegerPos
pCon            ::   UU.Parser Token (DTerm String)
pCon            =    (toDTerm id)   UU.<$> pConidPos
pVar            ::   UU.Parser Token (DTerm String)
pVar            =    (toDTerm id)   UU.<$> pVaridPos
pOp             ::   UU.Parser Token (DTerm String)
pOp             =    (toDTerm id)   UU.<$> pVarsymPos

pTerm           ::  (UU.IsParser p Token) 
                =>  String -> p (DTerm String)
pTerm t         =   (toDTerm id . (\loc -> (t,loc)))  UU.<$> pKeyPos t

newtype Const f a s = C {unC :: f a}


compile :: Grammar a -> UU.Parser Token a
compile (Grammar (start :: Ref a env) rules) 
                       = unC (lookupEnv start result)
  where  result  =  
          mapEnv 
          (\ (PS ps) -> C (foldr1 (UU.<|>) [ comp p | p <- ps]))
          rules

         comp :: forall t . Prod NF t env -> UU.Parser Token t

         comp (Star     x y)   = comp x UU.<*>   comp y
         comp (FlipStar x y)   = comp x UU.<**>  comp y
         comp (Pure     x)     = UU.pLow x

         comp (Sym (Term t))   = pTerm t
         comp (Sym (Nont n))   = unC (lookupEnv n result)

         comp (Sym TermInt)    = pInt
         comp (Sym TermChar)   = pChr
         comp (Sym TermVarid)  = pVar
         comp (Sym TermConid)  = pCon
         comp (Sym TermOp)     = pOp


mapEnv  ::  (forall a . f a s -> g a s)  
        ->  Env f s env -> Env g s env
mapEnv  _ Empty       = Empty
mapEnv  f (Ext r v)   = Ext (mapEnv f r) (f v)


-- PARSE ----------------------------------------------------------------------

type ParseMsg = UU.Message Token (Maybe Token)

data ParseResult a = Ok  a
                   | Rep a [ParseMsg] 
      deriving Show

parse :: UU.Parser Token a -> [Token] -> ParseResult a
parse p input = case rparse p input of
                  (a,[]  ) -> Ok a
                  (a,msgs) -> Rep a msgs


rparse :: UU.Parser Token a -> [Token] -> (a, [ParseMsg])
rparse p input = let (UU.Pair a _,msgs) =  eval (UU.parse p input)
                 in (a,msgs)
 where eval :: UU.Steps a Token (Maybe Token) -> (a, [ParseMsg])
       eval (UU.OkVal v        r) = let (a,msgs) = v `seq` eval r 
                                    in  (v a,msgs)
       eval (UU.Ok             r) = eval r
       eval (UU.Cost  _        r) = eval  r
       eval (UU.StRepair _ msg r) = let (v,msgs) = eval r 
                                    in  (v,msg:msgs)
       eval (UU.Best _   r     _) = eval  r
       eval (UU.NoMoreSteps v   ) = (v,[]) 


{-# OPTIONS -XExistentialQuantification -XArrows -XDoRec -XGADTs #-}

module Language.Grammars.Transformations.LeftCorner (leftCorner) where

import Language.AbstractSyntax.TTTAS
import Language.Grammars.Grammar
import Language.Grammars.Transformations.GramTrafo
import Control.Arrow


leftCorner :: forall a . Grammar a -> Grammar a
leftCorner (Grammar start productions)
      = case runTrafo (lctrafo productions) Unit () of
            Result _ (T tt) gram -> 
                 Grammar (tt start) gram

lctrafo :: GramEnv env env 
        -> Trafo Unit (Productions NF) s () (T env s)
lctrafo productions = proc _ ->
            do   rec  let tenv_s = map2trans menv_s
                      menv_s <- (rules1 productions productions) -< tenv_s
                 returnA -< tenv_s


rules1  ::  GramEnv env env
        ->  GramEnv env env' 
        ->  Trafo  Unit (Productions NF) s (T env s) 
                                           (Mapping env' s)
rules1 _ Empty          
     = proc _ ->
        returnA -< Mapping Empty  

rules1 productions (Ext ps (PS prods)) 
     = proc tenv_s ->
        do  p <- app_rule1 productions prods -< tenv_s
            r <- newSRef -< p   
            Mapping e <- rules1 productions ps -< tenv_s
            returnA -< Mapping (Ext e r)


app_rule1 :: forall env a s. GramEnv env env
          -> [Prod NF a env]
          -> Trafo Unit (Productions NF) s (T env s) (Productions NF a s)
app_rule1 productions prods =  initMap 
          ( proc tenv_s -> 
                do pss <- sequenceA  (map  (rule1 productions . accessLeftMost) prods) -< tenv_s 
                   returnA -< PS (concatMap unPS pss)
          )

rule1  :: GramEnv env env -> Prod NF a env 
       -> GramTrafo env a s (T env s) (Productions NF a s)

rule1 gram (Star (Sym x) beta) 
     = proc tenv_s ->
        do  insertS gram x -< (tenv_s, mapProd tenv_s beta)

rule1 gram (FlipStar (Sym x) beta) 
     = proc tenv_s ->
        do  insertF gram x -< (tenv_s, mapProd tenv_s beta)

rule1 gram (Sym x) 
     = rule1 gram $ FlipStar (Sym x) (Pure id)

rule1 _ (Pure _) 
     = error "Left-Corner(1): The grammar has empty productions."
rule1 _ _
     = error "Left-Corner(2): error in the transformation!!!"


accessLeftMost :: Prod NF a env -> Prod NF a env
accessLeftMost (Star      (Star      f g) h) 
  = accessLeftMost $ FlipStar f (Star (Star (Pure (\g' h' f' -> f' g' h'))   g) h)
accessLeftMost (Star      (FlipStar  f g) h) 
  = accessLeftMost $ FlipStar f (Star (Star (Pure flip)                      g) h)
accessLeftMost (FlipStar  (Star      f g) h) 
  = accessLeftMost $ FlipStar f (Star (Star (Pure (\g' h' f' -> h' (f' g'))) g) h)
accessLeftMost (FlipStar  (FlipStar  f g) h) 
  = accessLeftMost $ FlipStar f (Star (Star (Pure (\g' h' f' -> h' (g' f'))) g) h)
accessLeftMost p = p 

rule2S  :: GramEnv env env 
        -> Symbol (x->a) t env 
        -> GramTrafo env a s (T env s, Ref x s) 
                             (Productions NF a s)
rule2S gram (Nont b) 
     = case lookupEnv b gram of
          PS ps ->  proc (tenv_s, a_x) ->
                      do pss <- sequenceA  
                                (map  (rule2bS gram  . accessLeftMost) ps) -< (tenv_s, a_x)
                         returnA -< PS (concatMap unPS pss)



rule2bS  :: GramEnv env env 
         -> Prod NF (b->a) env 
         -> GramTrafo env a s (T env s, Ref b s) 
                              (Productions NF a s)

rule2bS gram (Star (Sym x) beta) 
     = proc (tenv_s, a_b) ->
         do insertF gram x -< (tenv_s, Star (Star (Pure (\b y bya -> bya b y)) (mapProd tenv_s beta)) (Sym $ Nont a_b) )

rule2bS gram (FlipStar (Sym x) beta) 
     = proc (tenv_s, a_b) ->
         do insertF gram x -< (tenv_s, Star (Star (Pure (\bya y b -> bya b y)) (mapProd tenv_s beta)) (Sym $ Nont a_b) )

rule2bS gram (Sym x) 
     = rule2bS gram $ FlipStar (Sym x) (Pure id)

rule2bS _ (Pure _)
     = error "Left-Corner(3): The grammar has empty productions."

rule2bS _ _
     = error "Left-Corner(4): error in the transformation!!!"

--

rule2F  :: GramEnv env env 
        -> Symbol x t env 
        -> GramTrafo env a s (T env s, Ref (x->a) s) 
                             (Productions NF a s)
rule2F _    (Term a) 
     = proc (_, a_x) ->
        do  returnA -< PS [rule2aF a a_x]
rule2F gram (Nont b) 
     = case lookupEnv b gram of
          PS ps ->  proc (tenv_s, a_x) ->
                      do pss <- sequenceA  
                                (map  (rule2bF gram  . accessLeftMost) ps) -< (tenv_s, a_x)
                         returnA -< PS (concatMap unPS pss)
rule2F _    TermInt 
     = proc (_, a_x) ->
        do  returnA -< PS [rule2a'F TermInt a_x]
rule2F _    TermChar 
     = proc (_, a_x) ->
        do  returnA -< PS [rule2a'F TermChar a_x]
rule2F _    TermVarid 
     = proc (_, a_x) ->
        do  returnA -< PS [rule2a'F TermVarid a_x]
rule2F _    TermConid 
     = proc (_, a_x) ->
        do  returnA -< PS [rule2a'F TermConid a_x]
rule2F _    TermOp 
     = proc (_, a_x) ->
        do  returnA -< PS [rule2a'F TermOp a_x]
rule2F _    (TermAnyOf x)
     = proc (_, a_x) ->
        do  returnA -< PS [rule2a'F (TermAnyOf x) a_x]
rule2F _    (TermAnyExcept x)
     = proc (_, a_x) ->
        do  returnA -< PS [rule2a'F (TermAnyExcept x) a_x]



rule2a'F :: Symbol a t s -> Ref (a->b) s -> Prod NF b s
rule2a'F s refA_a
     =  FlipStar (Sym s) (Sym $ Nont refA_a) -- (flip ($)) <$> (Sym s) <*> nont refA_a


rule2aF :: String -> Ref (DTerm String -> a) s -> Prod NF a s
rule2aF a refA_a
     =  FlipStar (Sym $ Term a) (Sym $ Nont refA_a) -- (flip ($)) <$> term a <*> nont refA_a


rule2bF  :: GramEnv env env 
         -> Prod NF b env 
         -> GramTrafo env a s (T env s, Ref (b -> a) s) 
                              (Productions NF a s)

rule2bF gram (Star (Sym x) beta) 
     = proc (tenv_s, a_b) ->
         do insertF gram x -< (tenv_s, Star (Star (Pure (\b xa bx -> xa (bx b))) (mapProd tenv_s beta)) (Sym $ Nont a_b) )

rule2bF gram (FlipStar (Sym x) beta) 
     = proc (tenv_s, a_b) ->
         do insertF gram x -< (tenv_s, Star (Star (Pure (flip (.))) (mapProd tenv_s beta)) (Sym $ Nont a_b) )

rule2bF gram (Sym x) 
     = rule2bF gram $ FlipStar (Sym x) (Pure id)

rule2bF _ (Pure _)
     = error "Left-Corner(5): The grammar has empty productions."

rule2bF _ _
     = error "Left-Corner(6): error in the transformation!!!"

insertS ::  forall x t env s a
        .   GramEnv env env 
        ->  Symbol (x->a) t env
        ->  GramTrafo env a s  (T env s, Prod NF x s)
                               (Productions NF a s)
insertS gram x = 
     Trafo (
           \(MapA_X ms mf) -> case ms x of
                       Just r   -> extendA_X (MapA_X ms mf) r
           
                       Nothing  -> let  Trafo step = insertNewA_X 
                                   in   step (MapA_X ms mf)
           )
     where
       insertNewA_X = proc (tenv_s,p) ->
                     do  r <- newNontRS x  -< PS [p]
                         rule2S gram x     -< (tenv_s,r)


insertF ::  forall x t env s a
        .   GramEnv env env 
        ->  Symbol x t env
        ->  GramTrafo env a s  (T env s, Prod NF (x->a) s)
                               (Productions NF a s)
insertF gram x = 
     Trafo (
           \(MapA_X ms mf) -> case mf x of
                       Just r   -> extendA_X (MapA_X ms mf) r
           
                       Nothing  -> let  Trafo step = insertNewA_X 
                                   in   step (MapA_X ms mf)
           )
     where
       insertNewA_X = proc (tenv_s,p) ->
                     do  r <- newNontRF x  -< PS [p]
                         rule2F gram x     -< (tenv_s,r)


extendA_X  :: m env2 -> Ref x env2 
            -> TrafoE m (Productions NF) s env2 (t, Prod NF x s) (Productions NF a env)
extendA_X m r = fmap  (const $ PS []) $ 
                       updateSRef m r (\(_,p) (PS ps) -> PS (p:ps))


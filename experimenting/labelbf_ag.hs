{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction , TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}


-- UUAGC2AspectAG 0.9.42.3 (labelbf_ag.ag)

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive
import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1


{-- AspectAG Code --}

-- datatypes
data Tree = Node {nvalue_Tree_Node :: Int, left_Tree_Node :: Tree, right_Tree_Node :: Tree}
             | 
            Tip {tvalue_Tree_Tip :: Int}
  deriving (Show)
-- labels
nt_Tree = proxy :: Proxy Tree
data Ch_nvalue_Tree_Node; ch_nvalue_Tree_Node = proxy :: Proxy (Ch_nvalue_Tree_Node, Int)
data Ch_left_Tree_Node; ch_left_Tree_Node = proxy :: Proxy (Ch_left_Tree_Node, Tree)
data Ch_right_Tree_Node; ch_right_Tree_Node = proxy :: Proxy (Ch_right_Tree_Node, Tree)
data Ch_tvalue_Tree_Tip; ch_tvalue_Tree_Tip = proxy :: Proxy (Ch_tvalue_Tree_Tip, Int)
-- attributes
data Att_loc; att_loc = proxy :: Proxy Att_loc
data Att_inh; att_inh = proxy :: Proxy Att_inh
data Att_syn; att_syn = proxy :: Proxy Att_syn
data InhG_Tree = InhG_Tree { starts_InhG_Tree ::[Int] }
data SynG_Tree = SynG_Tree { ends_SynG_Tree ::[Int], labeled_SynG_Tree ::Tree }
-- rules
nts_group = nt_Tree .*. hNil
----Tree
inst_Tree_Node = emptyRule 
loc_Tree_Node = locdefM att_loc $
    do 
     loc <- at loc
     lhs <- at lhs
     nvalue <- at ch_nvalue_Tree_Node
     left <- at ch_left_Tree_Node
     right <- at ch_right_Tree_Node
     return $
      ()
inh_Tree_Node = inhdefM att_inh nts_group $
    do 
     loc <- at loc
     lhs <- at lhs
     nvalue <- at ch_nvalue_Tree_Node
     left <- at ch_left_Tree_Node
     right <- at ch_right_Tree_Node
     return $
      ch_left_Tree_Node.=.
       InhG_Tree {
        starts_InhG_Tree = tail (starts_InhG_Tree (lhs # att_inh)) 
       } .*. 
      ch_right_Tree_Node.=.
       InhG_Tree {
        starts_InhG_Tree = (ends_SynG_Tree (left # att_syn)) 
       } .*. 
      emptyRecord
syn_Tree_Node = syndefM att_syn $
    do 
     loc <- at loc
     lhs <- at lhs
     nvalue <- at ch_nvalue_Tree_Node
     left <- at ch_left_Tree_Node
     right <- at ch_right_Tree_Node
     return $
      SynG_Tree {
       ends_SynG_Tree = (head (starts_InhG_Tree (lhs # att_inh)) + 1) : (ends_SynG_Tree (right # att_syn)) , labeled_SynG_Tree = Node (head (starts_InhG_Tree (lhs # att_inh)) )
                                                                                                                                      (labeled_SynG_Tree (left # att_syn)) 
                                                                                                                                      (labeled_SynG_Tree (right # att_syn)) 
      }
inst_Tree_Tip = emptyRule 
loc_Tree_Tip = locdefM att_loc $
    do 
     loc <- at loc
     lhs <- at lhs
     tvalue <- at ch_tvalue_Tree_Tip
     return $
      ()
inh_Tree_Tip = inhdefM att_inh nts_group $
    do 
     loc <- at loc
     lhs <- at lhs
     tvalue <- at ch_tvalue_Tree_Tip
     return $
      emptyRecord
syn_Tree_Tip = syndefM att_syn $
    do 
     loc <- at loc
     lhs <- at lhs
     tvalue <- at ch_tvalue_Tree_Tip
     return $
      SynG_Tree {
       ends_SynG_Tree = let (x:xs) = (starts_InhG_Tree (lhs # att_inh)) 
                        in (x + 1) : xs, labeled_SynG_Tree = Tip (head (starts_InhG_Tree (lhs # att_inh)) )
      }
-- catas
----Tree
atts_Tree_Node = inh_Tree_Node `ext` inst_Tree_Node `ext` loc_Tree_Node `ext` syn_Tree_Node
semP_Tree_Node = knit atts_Tree_Node
atts_Tree_Tip = inh_Tree_Tip `ext` inst_Tree_Tip `ext` loc_Tree_Tip `ext` syn_Tree_Tip
semP_Tree_Tip = knit atts_Tree_Tip
-- semantic functions
----Tree
type T_Tree = (Record (HCons (LVPair (Proxy Att_inh) InhG_Tree) HNil)) -> (Record (HCons (LVPair (Proxy Att_syn) SynG_Tree) HNil))
-- instance SemType T_Tree Tree
-- sem_Tree :: Tree -> T_Tree
sem_Tree_Node _nvalue _left _right = semP_Tree_Node (ch_nvalue_Tree_Node .=. _nvalue .*. ch_left_Tree_Node .=. _left .*. ch_right_Tree_Node .=. _right .*. emptyRecord)
sem_Tree_Tip _tvalue = semP_Tree_Tip (ch_tvalue_Tree_Tip .=. _tvalue .*. emptyRecord)
{-# LINE 33 ".\\labelbf_ag.ag" #-}

testTree :: Tree
testTree =  Node 2 
                (Tip 3) 
                (Node 1 
                    (Tip 4) 
                    (Tip 3)
                )

test :: Tree
test = let (xs, t) = sem_Tree testTree (0:xs) in t
{-# LINE 126 "labelbf_ag.hs" #-}
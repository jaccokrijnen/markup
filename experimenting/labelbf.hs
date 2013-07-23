data Tree a = Node a (Tree a) (Tree a) | Leaf a
    deriving Show

n = Node
l = Leaf 0
t :: Tree Int
t = n 3 (n 1 l l) (n 1 (n 4 l l) l)


label :: Tree a -> Tree Int
label t = let (t', xs) = lab t (0:xs)
          in  t'
    where lab (Node _ l r) (x:xs) = let (l', xs')  = lab l xs
                                        (r', xs'') = lab r xs'
                                    in  (Node x l' r', x+1:xs'')
          lab (Leaf _    ) (x:xs) =     (Leaf x      , x+1:xs  )
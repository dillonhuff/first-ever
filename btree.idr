module BTree

data BTree a = Node2 a (BTree a) (BTree a) | Node1 a (BTree a) | Leaf a

insert : Ord a => a -> BTree a -> BTree a
insert x (Leaf y) = if x > y
       then (Node1 x (Leaf y))
       else (Node1 y (Leaf x))
insert x (Node1 a c) = if x < a
       then (Node1 a (insert x c))
       else (Node2 a c (Leaf x))
insert x (Node2 a l r) = if x < a
       then (Node2 a (insert x l) r)
       else (Node2 a l (insert x r))
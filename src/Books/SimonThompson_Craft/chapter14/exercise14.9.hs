data NTree = NilT | Node Integer NTree NTree deriving (Eq, Show)

tree1, tree2 :: NTree
tree1 = Node 10 NilT NilT
tree2 = Node 17 (Node 14 NilT NilT) (Node 20 NilT NilT)
tree3 = Node 3 (Node 4 NilT NilT) NilT


left :: NTree -> NTree
left NilT = NilT
left (Node n t1 t2) = t1

right :: NTree -> NTree
right NilT = NilT
right (Node n t1 t2) = t2

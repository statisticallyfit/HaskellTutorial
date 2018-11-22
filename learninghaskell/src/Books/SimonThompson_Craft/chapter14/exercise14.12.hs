data NTree = NilT | Node Integer NTree NTree deriving (Eq, Show)

t1, t2 :: NTree
t1 = Node 10 NilT NilT
t2 = Node 17 (Node 14 NilT NilT) (Node 20 NilT NilT)
t3 = Node 3 (Node 4 NilT NilT) NilT
t4 = Node 3 (Node 2 (Node 1 NilT NilT) (Node 25 NilT NilT)) (Node 4 NilT NilT)

bt123 = Node 2 (Node 1 NilT NilT) (Node 3 NilT NilT)
bt567 = Node 6 (Node 5 NilT NilT) (Node 7 NilT NilT)
btMain = Node 4 bt123 bt567



reflect :: NTree -> NTree
reflect NilT = NilT
reflect (Node n t1 t2) = Node n (reflect t2) (reflect t1)



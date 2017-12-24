data NTree = NilT | Node Integer NTree NTree deriving (Eq, Show)

t1, t2 :: NTree
t1 = Node 10 NilT NilT
t2 = Node 17 (Node 14 NilT NilT) (Node 20 NilT NilT)
t3 = Node 3 (Node 4 NilT NilT) NilT


isElem :: Integer -> NTree -> Bool
isElem n NilT = False
isElem elem (Node nodeNum t1 t2)
    | elem == nodeNum = True
    | otherwise = isElem elem t1 || isElem elem t2
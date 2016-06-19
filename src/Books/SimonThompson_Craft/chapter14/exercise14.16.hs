import Data.List hiding (sort)


data NTree a = NilT | Node a (NTree a) (NTree a) deriving (Eq, Show)

tree1, tree2 :: NTree Integer
tree1 = Node 10 NilT NilT
tree2 = Node 17 (Node 14 NilT NilT) (Node 20 NilT NilT)
tree3 = Node 3 (Node 4 NilT NilT) NilT


left :: NTree a -> NTree a
left NilT = NilT
left (Node n t1 t2) = t1

right :: NTree a -> NTree a
right NilT = NilT
right (Node n t1 t2) = t2


------------------------------------------------
isElem :: Eq a => a -> NTree a -> Bool
isElem n NilT = False
isElem elem (Node n t1 t2)
    | elem == n = True
    | otherwise = isElem elem t1 || isElem elem t2


-------------------------------------------------


maxNode :: NTree Integer -> Maybe Integer
maxNode tree = case maxN tree of
                    -999 -> Nothing
                    num -> Just num

maxN :: NTree Integer -> Integer
maxN NilT = -999
maxN (Node n t1 t2) = maximum [n, maxN t1, maxN t2]




minNode :: NTree Integer -> Maybe Integer
minNode tree = case minN tree of
                    -999 -> Nothing
                    num -> Just num

minN :: NTree Integer -> Integer
minN NilT = 999
minN (Node n t1 t2) = minimum [n, minN t1, minN t2]

-------------------------------------------------

bt123 = Node 2 (Node 1 NilT NilT) (Node 3 NilT NilT)
bt567 = Node 6 (Node 5 NilT NilT) (Node 7 NilT NilT)
btMain = Node 4 bt123 bt567



reflect :: NTree a -> NTree a
reflect NilT = NilT
reflect (Node n t1 t2) = Node n (reflect t2) (reflect t1)

-------------------------------------------------

collapse :: NTree a -> [a]
collapse NilT = []
collapse (Node n t1 t2) = collapse t1 ++ [n] ++ collapse t2


sort :: Ord a => NTree a -> [a]
sort tree = foldr insert [] flatTree
    where flatTree = collapse tree



data GTree a = Leaf a | Gnode [GTree a] deriving (Eq, Show)


t13 :: GTree Int
t1 = Leaf 2
t13 = Gnode [Leaf 1, Leaf 3]
t12345 = Gnode [Gnode [Leaf 1, Leaf 2], tmidlayer, Gnode [Leaf 4, Leaf 5]]
t1245 =  Gnode [Gnode [Leaf 1, Leaf 2], Gnode [Leaf 4, Leaf 5]]
tlayers1 = Gnode [Gnode [Leaf 7, Gnode [Leaf 3, Gnode [Leaf 111, Gnode [Gnode [Leaf 8]]]]]]
tlayers2 = Gnode [Gnode [Gnode [Leaf 3]]]
tlayers3 = Gnode [Gnode [Gnode []]]
tlayers4 = Gnode [Gnode [Leaf 10]]
tmidlayer = Gnode [Gnode [Leaf 7, Gnode [Leaf 3]]]


countLeaves :: GTree a -> Integer
countLeaves (Leaf _) = 1
countLeaves (Gnode (t:ts)) = countL [t] + countL ts

countL :: [GTree a] -> Integer
countL [Leaf _] = 1
countL [Gnode []] = 0
countL [Gnode [t]] = countL [t]
countL [Gnode (t:ts)] = countL [t] + countL ts


{-
depth :: GTree a -> Integer
depth (Leaf _) = 1 -- note leaf will count as extra layer compared to no leaf at edge
depth (Gnode (t:ts)) = max (depth t) (depth ts)


sumGTree :: GTree Int -> Int
sumGTree (Leaf n) = n
sumGTree (Gnode (t:ts)) = sumGTree t + sumGTree ts


occurs :: Eq a => a -> GTree a -> Bool
occurs elem (Leaf n) = elem == n
occurs elem (Gnode (t:ts)) = occurs elem t || occurs elem ts


mapGTree :: (a -> b) -> GTree a -> GTree b
mapGTree f (Leaf n) = Leaf (f n)
mapGTree f (Gnode (t:ts)) = Gnode (mapGTree f t) (mapGTree f ts)


flattenGTree :: GTree a -> [a]
flattenGTree (Leaf n) = [n]
flattenGTree (Gnode (t:ts)) = flattenGTree t ++ flattenGTree ts
-}

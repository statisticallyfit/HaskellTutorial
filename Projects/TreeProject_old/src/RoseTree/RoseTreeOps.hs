module RoseTreeOps where



data RoseTree a = Petal a | Briar a [RoseTree a] deriving (Eq, Show)


{-

data GTree a = Leaf a | Gnode [GTree a] deriving (Eq, Show)

mapGTree :: (a -> b) -> GTree a -> GTree b
mapGTree f (Leaf n) = Leaf (f n)
mapGTree f (Gnode ts) = Gnode (map (mapGTree f) ts)


-----------------------------------------------------
collapseGTree :: GTree a -> [a]
collapseGTree (Leaf n) = [n]
collapseGTree (Gnode ts) = concat $ map collapseGTree ts



propMapCollapse :: Eq b => (a -> b) -> GTree a -> Bool
propMapCollapse f tree = map f (collapseGTree tree) == collapseGTree (mapGTree f tree)

-}
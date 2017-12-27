
{- -- this is a search tree since flattened, it is sorted.
            5
           / \
          /   \
         /     \
       3         7
      / \       / \
    1     4   6     9

-}

data Tree = Leaf Int | Node Tree Int Tree deriving (Eq, Ord, Show)

t :: Tree
t =   Node    (Node (Leaf 1) 3 (Leaf 4))    5     (Node (Leaf 6) 7 (Leaf 9))


{-
occursInSearchTree :: Int -> Tree -> Bool
occursInSearchTree m (Leaf n) = m == n
occursInSearchTree m (Node l n r)
    | m == n    = True
    | m < n     = occurs m l
    | otherwise = occurs m r

-}


occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r) = case compare m n of
    LT -> occurs m l
    EQ -> True
    GT -> occurs m r

{-
NOTE meaning?
This version is more eﬃcient because it only requires one comparison for each node,
whereas the previous version may require two comparisons.
-}

main = do
    print $ occurs 1 t
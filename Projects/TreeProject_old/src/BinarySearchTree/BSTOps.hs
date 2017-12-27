module BST where

data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
    deriving (Eq)


instance Show a => Show (Tree a) where
    show tree = draw 1 "\n" tree
        where
        draw _ _ Empty = "Empty"
        draw _ _ (Leaf n) = "Leaf " ++ show n
        draw count indent (Node n left right)
            = "Node " ++ (show n) ++ indent' ++ draw count' indent' left
            ++ indent' ++ draw count' indent' right
            where
            indent' = indent ++ "    "
            count' = count + 1



t1 = Node 4 (Node 2 (Leaf 1) (Leaf 3)) (Node 6 (Leaf 5) (Leaf 7))
t2 = Node 9 (Leaf 8) (Node 11 (Leaf 10) (Leaf 12))
{-
t1 = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))
            (Node 6 (Node 5 Empty Empty) (Node 7 Empty Empty))

t2 = Node 9 (Node 8 Empty Empty)
            (Node 11 (Node 10 Empty Empty) (Node 12 Empty Empty))
-}

{-

-- NOTE These are just for binary search tree!
insert :: Ord a => a -> Tree a -> Tree a
insert x Nil = Leaf x
insert x (Leaf n)
    | n == x = Leaf n
    | n > x  = Node n (Leaf x) Nil
    | n < x  = Node n Nil (Leaf x)
insert x (Node n left right)
    | n == x = Node n left right
    | x > n  = Node n left (insert x right)
    | x < n  = Node n (insert x left) right

-}

delete :: Ord a => a -> Tree a -> Tree a
delete x (Leaf n) = if x == n then Empty else Leaf n
delete x (Node n left right)
    | x < n   = Node n (delete x left) right
    | x > n   = Node n left (delete x right)
    | isEmpty right = left -- so in these leftover 3 tests (val == v)
    | isEmpty left  = right
    | isLeaf right = if (x == (leafVal right)) then left else right
    | isLeaf left = if (x == (leafVal left)) then right else left
    | otherwise = join left right


-- note is auxiliary, not exported.
-- postcondition: all elements on left are smaller than those on right.
join :: Ord a => Tree a -> Tree a -> Tree a
join left right = Node miniVal left newTree
    where (Just miniVal) = minTree right
          newTree = delete miniVal right


minTree :: Ord a => Tree a -> Maybe a
minTree t
    | isEmpty t = Nothing
    | isEmpty left = Just n
    | isLeaf left = if (n < nLeaf) then (Just n) else (Just nLeaf)
    | otherwise = minTree left
    where left = leftSub t
          n = treeVal t
          nLeaf = leafVal left


leftSub :: Tree a -> Tree a
leftSub (Node _ left _) = left
leftSub _ = error "leftSub"

isLeaf :: Tree a -> Bool
isLeaf (Leaf x) = True
isLeaf _ = False

leafVal :: Tree a -> a
leafVal (Leaf x) = x
leafVal _ = error "leafVal"

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

treeVal :: Tree a -> a
treeVal (Node n _ _) = n
treeVal _ = error "treeVal"







insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Leaf x
insert x (Leaf n)
    | x < n = Node n (Leaf x) Empty
    | x > n = Node n Empty (Leaf x)
    | otherwise = Leaf n
insert x tree@(Node n left right)
    | x < n = Node n (insert x left) right
    | x > n = Node n left (insert x right)
    | otherwise = tree

build :: Ord a => [a] -> Tree a
build xs = foldl (flip insert) Empty xs

build' :: Ord a => [a] -> Tree a
build' [] = Empty
build' (x:xs) = insert x (build' xs)

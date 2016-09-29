

data BST a = Nil | Leaf a | Node a (BST a) (BST a)
    deriving (Eq, Show)



-- deleting 3 from these trees.
testDelete1 = Node 2 (Node 1 Nil Nil) (Node 3 Nil (Node 4 Nil Nil))
testDelete2 = Node 1 (Node 0 Nil Nil) (Node 3 (Node 2 Nil Nil) Nil)
testDelete3 = Node 1 (Node 0 Nil Nil) (Node 3 (Node 2 Nil Nil) (Node 4 Nil Nil))


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



delete :: Ord a => a -> Tree a -> Tree a
delete x (Node n left right)
    | x < n   = Node n (delete x left) right
    | x > n   = Node n left (delete x right)
    | isNil right  = left -- so in these leftover 3 tests (val == v)
    | isNil left  = right
    | otherwise = join left right

-- note is auxiliary, not exported.
-- postcondition: all elements on left are smaller than those on right.
join :: Ord a => Tree a -> Tree a -> Tree a
join left right = Node miniVal left newTree
    where (Just miniVal) = minTree right
          newTree = delete miniVal right

-}
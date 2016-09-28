

data BinSearchTree a = Leaf | Node a (BinSearchTree a) (BinSearchTree a)
    deriving (Eq, Show)



insert :: Ord a => a -> BinSearchTree a -> BinSearchTree a
insert x Nil = Node x Nil Nil
insert x (Node n left right)
    | n == x = Node n left right
    | x > n  = Node n left (insert x right)
    | x < n  = Node n (insert x left) right


-- deleting 3 from these trees.
testDelete1 = Node 2 (Node 1 Nil Nil) (Node 3 Nil (Node 4 Nil Nil))
testDelete2 = Node 1 (Node 0 Nil Nil) (Node 3 (Node 2 Nil Nil) Nil)
testDelete3 = Node 1 (Node 0 Nil Nil) (Node 3 (Node 2 Nil Nil) (Node 4 Nil Nil))


delete :: Ord a => a -> Tree a -> Tree a
delete x (Node n left right)
    | x < n   = Node n (delete x left) right
    | x > n   = Node n left (delete x right)
    | isNil right  = left -- so in these leftover 3 tests (val == v)
    | isNil left  = right
    | otherwise = join left right
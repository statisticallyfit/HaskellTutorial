

data Tree = Leaf Int | Node Tree Tree deriving (Eq, Show)

-- HELp how to declare this tree with numbers at nodes? ???
t1 :: Tree
t1 = Node (Node (Leaf 1) (Leaf 4))    ( Node (Leaf 6)  (Node (Leaf 9) (Node (Leaf 6) (Leaf 7))) )
-- t =   Node    (Node (Leaf 1) 3 (Leaf 4))    5     (Node (Leaf 6) 7 (Leaf 9))

t2 :: Tree
t2 = Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 6) (Leaf 9))



balanced :: Tree -> Bool
balanced (Leaf _) = True
balanced (Node l r) = abs (numLeaves l - numLeaves r) <= 1 && balanced l && balanced r
-- HELP why this complicated definition? Why do I need to check balance of left and right?
-- not enough just the first part before the "&&s"?

-- balanced (Node l r) = numLeaves l == numLeaves r




numLeaves :: Tree -> Int
numLeaves (Leaf _) = 1
numLeaves (Node l r) = numLeaves l + numLeaves r


main = do
    print $ balanced t1
    print $ balanced t2
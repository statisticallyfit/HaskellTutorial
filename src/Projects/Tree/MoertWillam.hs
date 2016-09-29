module MoertelWillam where

data Tree a = Nil | Node a (Tree a) (Tree a)
    deriving (Eq, Show)


traverseTree ::
    (a -> (b -> b) -> (b -> b) -> b -> b)
     -- function step, that takes type a, two b->b funcs and seed of type b, returns b type.
            -> (t -> a)     -- function that converts type t to a (inner type of tree is t, like Tree Int)
            -> b            -- z
            -> Tree t       -- tree
            -> b            -- result
traverseTree step f z tree = go tree z
    where
        go Nil z = z
        go (Node x l r) z = step (f x) (go l) (go r) z

------------------------------------------------------
tree7 :: Tree Int
tree7 = Node 7
            (Node 1
                (Node 0 Nil Nil)
                (Node 3
                    (Node 2 Nil Nil)
                    (Node 5
                        (Node 4 Nil Nil)
                        (Node 6 Nil Nil))))
            (Node 9
                (Node 8 Nil Nil)
                (Node 10 Nil Nil))

tree15 :: Tree Int
tree15 = Node 8
            (Node 4
                (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil))
                (Node 5 (Node 6 Nil Nil) (Node 7 Nil Nil)))
            (Node 12
                (Node 10 (Node 9 Nil Nil) (Node 11 Nil Nil))
                (Node 14 (Node 13 Nil Nil) (Node 15 Nil Nil)))

preorder   = traverseTree $ \n l r -> r . l . n
inorder    = traverseTree $ \n l r -> r . n . l
postorder  = traverseTree $ \n l r -> n . r . l


flatten :: ((a -> [a] -> [a])   -> [t] -> b -> [c])    -> b -> [c]
flatten traversal = reverse . traversal (:) [] --tree arg here

minusTree traversal t = traversal (-) 0 t


printMinus :: Show a => a -> String -> String
printMinus x y = "(" ++ show x ++ "-" ++ y ++ ")"


testFlatPre t = flatten preorder t   -- [2,1,3]
testFlatIn t = flatten inorder t    -- [1,2,3]
testFlatPost t = flatten postorder t  -- [1,3,2]

testMinusPre t = minusTree preorder t
testMinusIn t = minusTree inorder t
testMinusPost t = minusTree postorder t

------------------------------------------------------

fold f z  Nil = z
fold f z (Node x l r) = f x (fold f z l) (fold f z r)

size'   = fold  (\x l r -> 1 + l + r)    0
height' = fold  (\x l r -> 1 + max l r)  0
mirror  = fold  (flip . Node)  Nil
map' f = fold  (Node . f) Nil

inFold  = fold  (\x l r -> l ++ x : r)    []
preFold = fold  (\x l r -> x : l ++ r)    []
postFold = fold (\x l r -> l ++ r ++ [x]) []

testFoldIn t = inFold t
testFoldPre t = preFold t
testFoldPost t = postFold t


------------------------------------------------------
leaf x = Node x Nil Nil



insert :: Ord a => a -> Tree a -> Tree a
insert x Nil = leaf x
insert x tree@(Node n left right) =
    if x < n then Node n (insert x left) right
    else if x > n then Node n left (insert x right)
    else tree

build :: Ord a => [a] -> Tree a
build xs = foldl (flip insert) Nil xs

build' :: Ord a => [a] -> Tree a
build' [] = Nil
build' (x:xs) = insert x (build' xs)

sortTree :: Ord a => [a] -> [a]
sortTree xs  = flatten inorder (build xs)

testBuild = build [7,2,9,1,8,4,4,5,3,7,6,1,10,9]
testBuild' = build' [7,2,9,1,8,4,4,5,3,7,6,1,10,9]
------------------------------------------------------

-- HELP how to search just in left or right subtree?
leftorder  = traverseTree $ \n l r -> l . n
rightorder = traverseTree $ \n l r -> r . n

treemin = leftorder min maxBound
treemax = rightorder max minBound

testLeft = treemin tree7 :: Int  -- 1
testRight = treemax tree7 :: Int  -- 3


------------------------------------------------------
-- s"Node($n, $pad${staircase(count + 1, pad, lft)}, $pad${staircase(count + 1, pad, rgt)})"
drawTree :: Show a => Tree a -> IO()
drawTree tree = putStrLn $ draw 1 "\n" tree
    where
    draw count indent Nil = "Nil"
    -- draw count indent (Node n Nil Nil) = "Leaf " ++ show n
    draw count indent (Node n Nil Nil) = "Node " ++ show n ++ " Nil Nil"
    draw count indent (Node n left right)
        = "Node " ++ (show n) ++ indent' ++ draw count' indent' left
                              ++ indent' ++ draw count' indent' right
        where
        indent' = indent ++ "    "
        count' = count + 1

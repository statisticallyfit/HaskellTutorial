module MW where

-- source:
-- http://www.willamette.edu/~fruehr/254/samples/Trees.hs
-- (more in evernote Scala Traversable Things)

import Prelude hiding (subtract)

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

preorder   = traverseTree $ \n l r -> r . l . n
inorder    = traverseTree $ \n l r -> r . n . l
postorder  = traverseTree $ \n l r -> n . r . l


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
------------------------------------------------------

collapse :: ((a -> [a] -> [a])   -> [t] -> b -> [c])    -> b -> [c]
collapse traversal = reverse . traversal (:) [] --tree arg here

subtract :: (Num a, Num b) => ((a -> a -> a) -> b -> c -> d) -> c -> d
subtract traversal t = traversal (-) 0 t

add :: (Num a, Num b) => ((a -> a -> a) -> b -> c -> d) -> c -> d
add traversal t = traversal (+) 0 t

-- HELP to how use this with the preorder/inorder/postorder functions?
printMinus :: Show a => a -> String -> String
printMinus x y = "(" ++ show x ++ "-" ++ y ++ ")"


testFlatPre t = collapse preorder t   -- [2,1,3]
testFlatIn t = collapse inorder t    -- [1,2,3]
testFlatPost t = collapse postorder t  -- [1,3,2]

testSubPre t = subtract preorder t
testSubIn t = subtract inorder t
testSubPost t = subtract postorder t

testAddPre t = add preorder t
testAddIn t = add inorder t
testAddPost t = add postorder t

------------------------------------------------------
-- NOTE this is inorder fold
fold :: (a -> b -> b -> b) -> b -> Tree a -> b
fold f z  Nil = z
fold f z (Node x l r) = f x (fold f z l) (fold f z r)


-- TODO testing that foldr list == this below way of getting the answer.
size'   = fold  (\x l r -> 1 + l + r)    0
height' = fold  (\x l r -> 1 + max l r)  0
mirror  = fold  (flip . Node)  Nil
map' f = fold  (Node . f) Nil

inFoldFlat t = fold  (\x l r -> l ++ x : r)     [] t
preFoldFlat t = fold  (\x l r -> x : l ++ r)    [] t
postFoldFlat t = fold (\x l r -> l ++ r ++ [x]) [] t

inFoldSub t = fold (\x lx rx -> (lx - x) - rx)   0 t
preFoldSub t = fold (\x lx rx -> (x - lx) - rx)  0 t
postFoldSub t = fold (\x lx rx -> (lx - rx) - x) 0 t


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

sort :: Ord a => [a] -> [a]
sort xs  = collapse inorder (build xs)

xs = [7,2,9,1,8,4,4,5,3,7,6,1,10,9]
testBuild = build xs
testBuild' = build' xs
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

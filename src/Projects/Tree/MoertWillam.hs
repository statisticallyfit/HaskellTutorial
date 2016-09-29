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


preorder   = traverseTree $ \n l r -> r . l . n
inorder    = traverseTree $ \n l r -> r . n . l
postorder  = traverseTree $ \n l r -> n . r . l


flatten :: ((a -> [a] -> [a])   -> [t] -> b -> [c])    -> b -> [c]
flatten traversal = reverse . traversal (:) [] --tree arg here

minusTree traversal tree = traversal (-) 0 tree


printMinus :: Show a => a -> String -> String
printMinus x y = "(" ++ show x ++ "-" ++ y ++ ")"


testFlatPre = flatten preorder tree7   -- [2,1,3]
testFlatIn = flatten inorder tree7    -- [1,2,3]
testFlatPost = flatten postorder tree7  -- [1,3,2]

testMinusPre = minusTree preorder tree7
testMinusIn = minusTree inorder tree7
testMinusPost = minusTree postorder tree7

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

testFoldIn = inFold tree7
testFoldPre = preFold tree7
testFoldPost = postFold tree7


------------------------------------------------------
leaf x = Node x Nil Nil

inord  Nil = []
inord  (Node x l r) = inord l ++ x : inord r

insert x Nil = leaf x
insert x (Node n l r) = if x < n then Node n (insert x l) r
                                 else Node n l (insert x r)

build xs = foldl (flip insert) Nil xs

tsort xs  = inord (build xs)

testBuild = build [0..10]
------------------------------------------------------

leftorder  = traverseTree $ \n l r -> l . n
rightorder = traverseTree $ \n l r -> r . n

treemin = leftorder min maxBound
treemax = rightorder max minBound

testLeft = treemin tree7 :: Int  -- 1
testRight = treemax tree7 :: Int  -- 3


------------------------------------------------------

drawTree :: Tree a -> String
drawTree tree = draw 1 "\n" tree
    where
        draw count pad Nil = "Nil"
        draw count pad (Node n left right)
            = "Node " + n + "
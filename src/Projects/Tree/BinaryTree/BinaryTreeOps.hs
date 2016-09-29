module BinaryTreeOps where

import Test.QuickCheck -- (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Control.Monad hiding (join)
import Data.Maybe
import Data.List (findIndex, elemIndex)


data Tree a = Nil | Leaf a | Node a (Tree a) (Tree a) deriving (Eq, Show)


t1, t2, t3, t4, t5, t6, t7 :: Tree Integer

t1 = Node 8 (Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil))
                    (Node 6 (Node 5 Nil Nil) (Node 7 Nil Nil)))
            (Node 10 (Node 9 Nil Nil) (Node 11 Nil Nil))

t2 = Node 8 (Node 4 (Node 2 Nil (Node 3 Nil Nil))
                    (Node 6 (Node 5 Nil Nil) (Node 7 Nil Nil)))
            (Node 10 (Node 9 Nil Nil) (Node 11 Nil Nil))

t3 = Node 5 (Node 3 (Node 1 Nil Nil) (Node 4 Nil Nil))
            (Node 13 (Node 7 Nil Nil) (Node 14 Nil (Node 17 Nil Nil)))

t4 = Nil

t5 = Node 5 (Node 2 Nil Nil) (Node 8 Nil Nil)

t6 = Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil))
            (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))

-- testing closest with different distanced nodes
t7 = Node 8 (Node 5  (Node 1 Nil Nil)  (Node 6 Nil Nil))
            (Node 20 (Node 19 Nil Nil) (Node 21 Nil Nil))

tdup = Node 8 (Node 8 (Node 7 Nil Nil) Nil) (Node 10 (Node 9 Nil Nil) Nil)

t123 = Node 2 (Node (-1) Nil Nil) (Node 3 Nil Nil)
t567 = Node (-6) (Node 5 Nil Nil) (Node (-7) Nil Nil)
t1_7 = Node (-4) t123 t567

-- deleting 3 from these trees.
testDelete1 = Node 2 (Node 1 Nil Nil) (Node 3 Nil (Node 4 Nil Nil))
testDelete2 = Node 1 (Node 0 Nil Nil) (Node 3 (Node 2 Nil Nil) Nil)
testDelete3 = Node 1 (Node 0 Nil Nil) (Node 3 (Node 2 Nil Nil) (Node 4 Nil Nil))

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



isNil :: Tree a -> Bool
isNil Nil = True
isNil _ = False

isNode :: Tree a -> Bool
isNode Nil = False
isNode _ = True

leftSub :: Tree a -> Tree a
leftSub (Node _ left _) = left
leftSub _ = error "leftSub"

rightSub :: Tree a -> Tree a
rightSub (Node _ _ right) = right
rightSub _ = error "rightSub"

treeVal :: Tree a -> a
treeVal Nil = error "treeVal"
treeVal (Leaf n) = n
treeVal (Node n _ _) = n


-- insert and delete are not inverse functions.
{-
todo insert at a specific spot! for normal binary tree (non_BST)
-}

{-
delete specific element at certain spot! (non-BST)
-}


minTree :: Ord a => Tree a -> Maybe a
minTree t
    | isNil t = Nothing
    | isNil left = Just n
    | otherwise = minTree left
    where left = leftSub t
          n = treeVal t


-- note return nth element of search tree.
-- HELP understand how the (n- leftSize - 1) thingy works.
-- It can be used in situations like index 3 t2 ==> 5.
index :: Int -> Tree a -> a
index x tree
    | isNil tree = error "index"
    | x < leftSize = index x left
    | x == leftSize = n
    | otherwise = index (x - leftSize - 1) right
    where n = treeVal tree
          left = leftSub tree
          right = rightSub tree
          leftSize = size left

size :: Tree a -> Int
size t
    | isNil t = 0
    | otherwise = 1 + size (leftSub t) + size (rightSub t)


-- note returns index of element.
-- precondition - val must occur in tree.
indexOf :: Ord a => a -> Tree a -> Int
indexOf x tree = fromJust $ elemIndex x (collapse tree)

collapse :: Tree a -> [a]
collapse Nil = []
collapse (Node n tree right) = collapse tree ++ [n] ++ collapse right


occurs :: Ord a => a -> Tree a -> Bool
occurs _ Nil = False
occurs x (Node n left right)
    | x == n = True
    | otherwise = occurs x left || occurs x right



-- precondition: given value must occur at least once in tree.
-- note betting that findIndex always returns Just not Nothig because precondition is
-- that val must occur at least once.
predecessor :: Ord a => a -> Tree a -> Maybe a
predecessor x Nil = Nothing
predecessor x tree
    | not $ occurs x tree       = Nothing
    | x == head (collapse tree) = Nothing
    | otherwise                   = Just $ list !! n
    where list = reverse $ collapse tree
          n = fromJust $ findIndex (< x) list


-- precondition: val must occur at least once in list. Duplicates allowed.
successor :: Ord a => a -> Tree a -> Maybe a
successor x Nil = Nothing
successor x tree
    | not $ occurs x tree = Nothing
    | x == last list      = Nothing
    | otherwise             = Just $ list !! n
    where list = collapse tree
          n = fromJust $ findIndex (> x) list



-- note returns value in t which has smallest numerical difference from v.
-- precondition: value val must occur in tree given.
closest :: Integer -> Tree Integer -> Maybe [Integer]
closest x Nil = Nothing
closest x t -- = if occurs val t then clos val t else Nothing
    | not $ occurs x t = Nothing
    | length allJusts == 1 = onlyOne
    | otherwise            = closerOne
    where (pm, sm)  = (predecessor x t, successor x t)
          allJusts  = filter isJust [pm, sm]
          onlyOne   = Just [fromJust (head allJusts)]
          vs@[p, s] = catMaybes allJusts -- both values p and s
          [d1, d2]  = map (\v -> abs(v - x)) vs
          closerOne
            | d1 == d2 = Just [p, s]
            | d1 < d2  = Just [p]
            | d1 > d2  = Just [s]




--- Traversals: Flattening

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Nil = Nil
mapTree f (Node n left right) = Node (f n) (mapTree f left) (mapTree f right)

--- These are all DFS methods (depth first search methods)
-- node, left, right
collapsePre :: Tree a -> [a]
collapsePre Nil = []
collapsePre (Node n left right)
    = [n] ++ collapsePre left ++ collapsePre right

-- left, node, right
collapseIn :: Tree a -> [a]
collapseIn Nil = []
collapseIn (Node n left right)
    = collapseIn left ++ [n] ++ collapseIn right

-- left, right, node (think: children first)
collapsePost :: Tree a -> [a]
collapsePost Nil = []
collapsePost (Node n left right)
    = collapsePost left ++ collapsePost right ++ [n]

--- bfs flatten (and general traversal action)
--- todo todo todo

--- todotodotodo: filter tree (leaving perhaps sparse tree or just more condensed tree Keep in mind
-- the structure of original tree)

-- todotodotodo: foldtree (pre, post, inorder) using foldr, foldl -- all the combinations!


--- alternative solution
flattenPre :: Tree a -> [a] -> [a]
flattenPre Nil accList = accList
flattenPre (Node n left right) accList = n : flattenPre left (flattenPre right accList)

flattenIn :: Tree a -> [a] -> [a]
flattenIn Nil accList = accList
flattenIn (Node n left right) accList = flattenIn left (n : (flattenIn right accList))

flattenPost :: Tree a -> [a] -> [a]
flattenPost Nil accList = accList
flattenPost (Node n left right) accList = flattenPost left (flattenPost right (n : accList))

collapsePre' :: Tree a -> [a]
collapsePre' tree = flattenPre tree []

collapseIn' :: Tree a -> [a]
collapseIn' tree = flattenIn tree []

collapsePost' :: Tree a -> [a]
collapsePost' tree = flattenPost tree []



--- Traversals: General folding

-- IMPORTANT even haskell designers say that foldr = post order and foldl = preorder!:
-- https://hackage.haskell.org/package/suffixtree-0.2.2.1/docs/src/Data-SuffixTree.html

-- note in foldl, the left has to be accumulator for the right tree.
-- n : flattenPre left (flattenPre right accList)
-- foldl f (foldl f (f z n) left) right
-- note in foldr, the right has to be accumulator for the left tree.
-- flattenPost left (flattenPost right (n : accList))
-- foldr f (f n (foldr f z right)) left
foldrPost :: (a -> b -> b) -> b -> Tree a -> b
foldrPost _ acc Nil = acc
foldrPost f acc (Node n left right)
    = foldrPost f (foldrPost f (f n acc) right) left
--preFoldr f (preFoldr f (f n acc) right) left

foldr2 _ acc Nil = acc
foldr2 f acc (Node n left right)
    = foldr2 f (foldr2 f (f n acc) left) right

foldr3 _ acc Nil = acc
foldr3 f acc (Node n left right)
    = f n (foldr3 f (foldr3 f acc left) right)

{-
foldrTry _ z Nil = z
foldrTry f z (Node es) = foldr (\(p,t) v -> f p (foldrTry f v t)) z es
-}


-- foldl f (foldl f (f z n) left) right
foldlPre :: (b -> a -> b) -> b -> Tree a -> b
foldlPre _ acc Nil = acc
foldlPre f acc (Node n left right)
    = foldlPre f (foldlPre f (f acc n) left) right

------------------------------------
-- inorder left ++ [n] ++ inorder right
-- flattenIn left (a : (flattenIn right accList))
foldIn :: (b -> a -> b -> b) -> b -> Tree a -> b
foldIn _ z Nil = z
foldIn f z (Node n l r)
    = f (foldIn f z l) n (foldIn f z r)

-- TODO testing that foldr list == this below way of getting the answer.
size'   = foldIn  (\x l r -> 1 + l + r)    0
height' = foldIn  (\x l r -> 1 + max l r)  0
mirror  = foldIn  (flip . Node)  Nil
map' f = foldIn  (Node . f) Nil

inFoldFlat t = foldIn  (\x l r -> l ++ x : r)     [] t
preFoldFlat t = foldIn  (\x l r -> x : l ++ r)    [] t
postFoldFlat t = foldIn (\x l r -> l ++ r ++ [x]) [] t

inFoldSub t = foldIn (\x lx rx -> (lx - x) - rx)   0 t
preFoldSub t = foldIn (\x lx rx -> (x - lx) - rx)  0 t
postFoldSub t = foldIn (\x lx rx -> (lx - rx) - x) 0 t

-- TODO implement preOrderFold then pass in the arguments... same for postorder

---------------------

printSumPreRight :: Show a => a -> String -> String
printSumPreRight x y = "(" ++ show x ++ "+" ++ y ++ ")"

printSumPreLeft :: Show a => String -> a -> String
printSumPreLeft x y = "(" ++ x ++ "+" ++ show y ++ ")"

printSumIn :: Show a => String -> a -> String -> String
printSumIn x y z = "(" ++ x ++ "+" ++ show y ++ "+" ++ z ++ ")"

------------------

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


testFlatPre t = collapse preorder t   -- [2,1,3]
testFlatIn t = collapse inorder t    -- [1,2,3]
testFlatPost t = collapse postorder t  -- [1,3,2]


collapse' :: ((a -> [a] -> [a])   -> [t] -> b -> [c])    -> b -> [c]
collapse' traversal = reverse . traversal (:) [] --tree arg here

------------------
t8 = Node 5
        (Node 3 (Node 1 Nil Nil) (Node 6 Nil Nil))
        (Node 9 (Node 8 Nil Nil) (Node 10 Nil Nil))

t15 = Node 8
            (Node 4
                (Node 2 (Leaf 1) (Leaf 3))
                (Node 5 (Leaf 6) (Leaf 7)))
            (Node 12
                (Node 10 (Leaf 9) (Leaf 11))
                (Node 14 (Leaf 13) (Leaf 15)))

-- note these are real preorder!
listFoldlPre = reverse $ foldlPre (flip(:)) [] t8 -- [5,3,1,6,9,8,10]
listFoldl =    reverse $ foldl (flip (:)) [] t8
-- note this one below is real postorder!!!
listFoldrPost = foldrPost (:) [] t8               -- [1,6,3,8,10,9,5]
listFoldr =     foldr (:) [] t8
listFoldr2 =    foldr2 (:) [] t8
listFoldrRealPost = foldr3 (:) [] t8


-- note preorder foldr HELP these are not the same
exampleFoldTreePRE_R t = foldrPost printSumPreRight "_" t
exampleFoldr t = foldr printSumPreRight "_" t
exampleFoldrRealPost t = foldr3 printSumPreRight "_" t
-- note preorder foldl (backwards)
exampleFoldTreePRE_L t = foldlPre printSumPreLeft "_" t
exampleFoldl t = foldl printSumPreLeft "_" t
-- note inorder
exampleFoldTreeIN t = foldIn printSumIn "_" t
--exampleFoldTreePOST =


--- Rewrite map using foldtree ------------------------------------------------------------

--- idea for later: construct a binary search tree using postorder/inorder/preorder
-- traversal.
-- note returns mapped list in swirly preorder traversal.
mapFoldTreeToList :: (a -> b) -> Tree a -> [b]
mapFoldTreeToList f tree = foldrPost ((:) . f) [] tree

-- HELP HELP HELP TODO map fold but return binary tree.
{-mapFoldTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapFoldTree f tree = foldrPreorder mk Leaf tree
    where mk a Leaf = Node Leaf (f a) Leaf
          mk a (Node l x r) = -}


draw :: Show a => Tree a -> IO()
draw tree = putStrLn $ drw 1 "\n" tree
    where
    drw count indent Nil = "Nil"
    -- draw count indent (Node n Nil Nil) = "Leaf " ++ show n
    drw count indent (Node n Nil Nil) = "Node " ++ show n ++ " Nil Nil"
    drw count indent (Node n left right)
        = "Node " ++ (show n) ++ indent' ++ drw count' indent' left
                              ++ indent' ++ drw count' indent' right
        where
        indent' = indent ++ "    "
        count' = count + 1


{-
TODO This is NEAT, very simple use to implement my folds
http://blog.moertel.com/posts/2012-01-26-the-inner-beauty-of-tree-traversals.html
http://www.willamette.edu/~fruehr/254/samples/Trees.hs
-}


---------------------------------------------------------------------------------------

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Node n left right) = Node (f n) (fmap f left) (fmap f right)


------ foldrPostorder f (foldrPostorder f (f n acc) left) right
instance Foldable Tree where
    foldMap _ Nil = mempty
    foldMap f (Node n left right) = (f n) <> (foldMap f left) <> (foldMap f right)

    foldl _ z Nil = z
    foldl f z (Node n left right) = foldl f (foldl f (f z n) left) right
    -- foldl f z (Leaf n) = f z n

    foldr _ z Nil = z
    foldr f z (Node n left right) = foldr f (f n (foldr f z right)) left
    -- foldr f z (Leaf n) = f n z




instance Traversable Tree where
    -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
    --traverse f (Leaf n) = Leaf <$> f n
    traverse _ Nil = pure Nil
    traverse f (Node n left right)
        = Node <$> f n  <*> traverse f left <*> traverse f right


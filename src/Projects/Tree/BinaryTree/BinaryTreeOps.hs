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
indexOf x tree = fromJust $ elemIndex x (collapseIn tree)


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
    | x == head (collapseIn tree) = Nothing
    | otherwise                   = Just $ list !! n
    where list = reverse $ collapseIn tree
          n = fromJust $ findIndex (< x) list


-- precondition: val must occur at least once in list. Duplicates allowed.
successor :: Ord a => a -> Tree a -> Maybe a
successor x Nil = Nothing
successor x tree
    | not $ occurs x tree = Nothing
    | x == last list      = Nothing
    | otherwise             = Just $ list !! n
    where list = collapseIn tree
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




--- Traversals: Flattening ------------------------------------------------------------------
--- bfs flatten (and general traversal action)
--- todo todo todo

--- todotodotodo: filter tree (leaving perhaps sparse tree or just more condensed tree Keep in mind
-- the structure of original tree)

-- todotodotodo: foldtree (pre, post, inorder) using foldr, foldl -- all the combinations!


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

------------------------------------
collapsePre' :: Tree a -> [a]
collapsePre' tree = flattenPre tree []
    where
    flattenPre Nil accList = accList
    flattenPre (Node n left right) accList
        = n : flattenPre left (flattenPre right accList)

collapseIn' :: Tree a -> [a]
collapseIn' tree = flattenIn tree []
    where
    flattenIn Nil accList = accList
    flattenIn (Node n left right) accList
        = flattenIn left (n : (flattenIn right accList))

collapsePost' :: Tree a -> [a]
collapsePost' tree = flattenPost tree []
    where
    flattenPost Nil accList = accList
    flattenPost (Node n left right) accList
        = flattenPost left (flattenPost right (n : accList))

---------------------------------------

--- Traversals: General folding

-- note in foldl, the left has to be accumulator for the right tree.
-- n : flattenPre left (flattenPre right accList)
-- foldl f (foldl f (f z n) left) right
-- note in foldr, the right has to be accumulator for the left tree.
-- flattenPost left (flattenPost right (n : accList))
-- foldr f (f n (foldr f z right)) left

-- note yields in order results
foldrIn :: (a -> b -> b) -> b -> Tree a -> b
foldrIn _ acc Nil = acc
foldrIn f acc (Node n left right) = foldrIn f (f n (foldrIn f acc right)) left

-- note yields in order results
foldlIn :: (b -> a -> b) -> b -> Tree a -> b
foldlIn _ acc Nil = acc
foldlIn f acc (Node n left right) = foldlIn f (f (foldlIn f acc left) n) right

foldrPost :: (a -> b -> b) -> b -> Tree a -> b
foldrPost _ acc Nil = acc
foldrPost f acc (Node n left right) = foldrPost f (foldrPost f (f n acc) right) left

foldlPost :: (b -> a -> b) -> b -> Tree a -> b
foldlPost _ acc Nil = acc
foldlPost f acc (Node n left right) = foldlPost f (foldlPost f (f acc n) right) left

foldrPre :: (a -> b -> b) -> b -> Tree a -> b
foldrPre _ acc Nil = acc
foldrPre f acc (Node n left right) = foldrPre f (foldrPre f (f n acc) left) right

-- foldl f (foldl f (f z n) left) right
foldlPre :: (b -> a -> b) -> b -> Tree a -> b
foldlPre _ acc Nil = acc
foldlPre f acc (Node n left right) = foldlPre f (foldlPre f (f acc n) left) right

{-
foldlIn :: (b -> a -> b -> b) -> b -> Tree a -> b
foldlIn f z  Nil = z
foldlIn f z (Node x l r) = f (foldlIn f z l) x (foldlIn f z r)-}

-- this is the same as foldrIn but with more arguments.
fold :: (a -> b -> b -> b) -> b -> Tree a -> b
fold f z  Nil = z
fold f z (Node x l r) = f x (fold f z l) (fold f z r)


-- TODO testing that foldr list == this below way of getting the answer.
sizeFold   = fold  (\x l r -> 1 + l + r)    0
depthFold = fold  (\x l r -> 1 + max l r)  0
mirror  = fold  (flip . Node)  Nil
mapFold f = fold  (Node . f) Nil

inFoldFlat t = fold  (\x l r -> l ++ x : r)     [] t
preFoldFlat t = fold  (\x l r -> x : l ++ r)    [] t
postFoldFlat t = fold (\x l r -> l ++ r ++ [x]) [] t

inFoldSub t = fold (\x lx rx -> (lx - x) - rx)   0 t
preFoldSub t = fold (\x lx rx -> (x - lx) - rx)  0 t
postFoldSub t = fold (\x lx rx -> (lx - rx) - x) 0 t

-- TODO implement preOrderFold then pass in the arguments... same for postorder

---------------------

printSumPreRight :: Show a => a -> String -> String
printSumPreRight x y = "(" ++ show x ++ "+" ++ y ++ ")"

printSumPreLeft :: Show a => String -> a -> String
printSumPreLeft x y = "(" ++ x ++ "+" ++ show y ++ ")"

printSumIn :: Show a => a -> String -> String -> String
printSumIn x y z = "((" ++ y ++ "-" ++ show x ++ ")" ++ "-" ++ z ++ ")"

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

foldTree :: a -> b -> (a -> b -> c -> t) -> c -> t
foldTree f z traversal t = traversal f z t

-- HELP todo
--printPre traversal t = traversal "+" "_" (map show t)

preorder, inorder, postorder :: (a -> b -> b) -> b -> Tree a -> b
preorder   = traverseTree $ \n l r -> n . l . r  -- r . l . n
inorder    = traverseTree $ \n l r -> l . n . r  -- r . n . l
postorder  = traverseTree $ \n l r -> l . r . n  -- n . r . l

------------------
collapse :: ((a -> [a] -> [a])   -> [t] -> b -> [c])    -> b -> [c]
collapse traversal = {-reverse . -}traversal (:) [] --tree arg here

sub :: (Num a, Num b) => ((a -> a -> a) -> b -> c -> t) -> c -> t
sub traversal t = traversal (-) 0 t

add traversal t = traversal (+) 0 t

flatPre = collapse preorder tree7
flatIn = collapse inorder tree7
flatPost = collapse postorder tree7 

subPre = sub preorder tree7
subIn = sub inorder tree7
subPost = sub postorder tree7

addPre = add preorder tree7
addIn = add inorder tree7
addPost = add postorder tree7

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
listFoldlPre = reverse $ foldlPre (flip(:)) [] tree7 -- [5,3,1,6,9,8,10]
listFoldl =    reverse $ foldl (flip (:)) [] tree7
-- note this one below is real postorder!!!
listFoldrPost = foldrPost (:) [] tree7                -- [1,6,3,8,10,9,5]
listFoldr =     foldr (:) [] tree7
--listFoldr2 =    foldr2 (:) [] tree7
--listFoldrRealPost = foldr3 (:) [] tree7


-- note preorder foldr HELP these are not the same
exampleFoldTreePRE_R t = foldrPost printSumPreRight "_" t
exampleFoldr t = foldr printSumPreRight "_" t
--exampleFoldrRealPost t = foldr3 printSumPreRight "_" t
-- note preorder foldl (backwards)
exampleFoldTreePRE_L t = foldlPre printSumPreLeft "_" t
exampleFoldl t = foldl printSumPreLeft "_" t
-- note inorder
exampleFoldTreeIN t = fold printSumIn "_" t
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


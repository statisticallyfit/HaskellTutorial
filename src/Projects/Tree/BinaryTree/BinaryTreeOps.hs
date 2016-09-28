module BinaryTreeOps where

import Test.QuickCheck -- (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Control.Monad hiding (join)
import Data.Maybe
import Data.List (findIndex, elemIndex)


data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq, Show)


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

t123 = Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)
t567 = Node 6 (Node 5 Nil Nil) (Node 7 Nil Nil)

-- deleting 3 from these trees.
testDelete1 = Node 2 (Node 1 Nil Nil) (Node 3 Nil (Node 4 Nil Nil))
testDelete2 = Node 1 (Node 0 Nil Nil) (Node 3 (Node 2 Nil Nil) Nil)
testDelete3 = Node 1 (Node 0 Nil Nil) (Node 3 (Node 2 Nil Nil) (Node 4 Nil Nil))



isNil :: Tree a -> Bool
isNil Nil = True
isNil _ = False

isNode :: Tree a -> Bool
isNode Nil = False
isNode _ = True

leftSub :: Tree a -> Tree a
leftSub Nil = error "leftSub"
leftSub (Node _ left _) = left

rightSub :: Tree a -> Tree a
rightSub Nil = error "rightSub"
rightSub (Node _ _ right) = right

treeVal :: Tree a -> a
treeVal Nil = error "treeVal"
treeVal (Node n _ _) = n


-- insert and delete are not inverse functions.
{-
todo insert at a specific spot! for normal binary tree (non_BST)
-}

{-
delete specific element at certain spot! (non-BST)
-}


insert :: Ord a => a -> Tree a -> Tree a
insert x Nil = Node x Nil Nil
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


minTree :: Ord a => Tree a -> Maybe a
minTree t
    | isNil t = Nothing
    | isNil left = Just n
    | otherwise = minTree left
    where left = leftSub t
          n = treeVal t

-- note is auxiliary, not exported.
-- postcondition: all elements on left are smaller than those on right.
join :: Ord a => Tree a -> Tree a -> Tree a
join left right = Node miniVal left newTree
    where (Just miniVal) = minTree right
          newTree = delete miniVal right



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
preorder :: Tree a -> [a]
preorder Nil = []
preorder (Node n left right) = [n] ++ preorder left ++ preorder right

-- left, node, right
inorder :: Tree a -> [a]
inorder Nil = []
inorder (Node n left right) = inorder left ++ [n] ++ inorder right

-- left, right, node (think: children first)
postorder :: Tree a -> [a]
postorder Nil = []
postorder (Node n left right) = postorder left ++ postorder right ++ [n]

--- bfs flatten (and general traversal action)
--- todo todo todo


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

preorder' :: Tree a -> [a]
preorder' tree = flattenPre tree []

inorder' :: Tree a -> [a]
inorder' tree = flattenIn tree []

postorder' :: Tree a -> [a]
postorder' tree = flattenPost tree []



--- Traversals: General folding


-- n : flattenPre left (flattenPre right accList)
preFoldr :: (a -> b -> b) -> b -> Tree a -> b
preFoldr _ acc Nil = acc
preFoldr f acc (Node n left right) = accRight
    where
    accRight = preFoldr f accLeft right
    accLeft = preFoldr f (f n acc) left


preFoldl :: (b -> a -> b) -> b -> Tree a -> b
preFoldl _ acc Nil = acc
preFoldl f acc (Node n left right) = accRight
    where
    accRight = preFoldl f accLeft right
    accLeft = preFoldl f (f acc n) left

-- inorder left ++ [n] ++ inorder right
-- flattenIn left (a : (flattenIn right accList))
inorderFold :: (b -> a -> b -> b) -> b -> Tree a -> b
inorderFold _ acc Nil = acc
inorderFold f acc (Node n left right)
    = f (inorderFold f acc left) n (inorderFold f acc right)


printSumPreRight :: Show a => a -> String -> String
printSumPreRight x y = "(" ++ show x ++ "+" ++ y ++ ")"

printPreLeft :: Show a => String -> a -> String
printPreLeft x y = "(" ++ x ++ "+" ++ show y ++ ")"

printSumIn :: Show a => String -> a -> String -> String
printSumIn x y z = "(" ++ x ++ "+" ++ show y ++ "+" ++ z ++ ")"

------------------

-- help help help todo how to implement foldtree using postorder traversal?
--- foldTree f (foldTree f (f x acc) left) right
-- flattenPost left (flattenPost right (a : accList))
{-
foldPostorder f acc (Node left x right)
    = foldPostOrder f left
-}

------------------
-- note preorder foldr
exampleFoldTreePRE_R = preFoldr printSumPreRight "_" t3
-- note preorder foldl (backwards)
exampleFoldTreePRE_L = preFoldl printPreLeft "_" t3
-- note inorder
exampleFoldTreeIN = inorderFold printSumIn "_" t3
--exampleFoldTreePOST =


--- Rewrite map using foldtree ------------------------------------------------------------

--- idea for later: construct a binary search tree using postorder/inorder/preorder
-- traversal.
-- note returns mapped list in swirly preorder traversal.
mapFoldTreeToList :: (a -> b) -> Tree a -> [b]
mapFoldTreeToList f tree = preFoldr ((:) . f) [] tree

-- HELP HELP HELP TODO map fold but return binary tree.
{-mapFoldTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapFoldTree f tree = foldrPreorder mk Leaf tree
    where mk a Leaf = Node Leaf (f a) Leaf
          mk a (Node l x r) = -}







---------------------------------------------------------------------------------------

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Node n left right) = Node (f n) (fmap f left) (fmap f right)


------ preFoldr f (preFoldr f (f n acc) left) right
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


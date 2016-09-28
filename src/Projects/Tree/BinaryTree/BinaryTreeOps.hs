module BinaryTreeOps where

import Test.QuickCheck -- (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Control.Monad hiding (join)
import Data.Maybe
import Data.List (findIndex, elemIndex)


data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq, Show)


t1 :: Tree Integer
t1 = Node 8 (Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil))
                    (Node 6 (Node 5 Nil Nil) (Node 7 Nil Nil)))
            (Node 10 (Node 9 Nil Nil) (Node 11 Nil Nil))

t2 :: Tree Integer
t2 = Node 8 (Node 4 (Node 2 Nil (Node 3 Nil Nil))
                    (Node 6 (Node 5 Nil Nil) (Node 7 Nil Nil)))
            (Node 10 (Node 9 Nil Nil) (Node 11 Nil Nil))
{-

t3 = Leaf
t4 = Node (Node Leaf 2 Leaf) 5 (Node Leaf 8 Leaf)
t5 = Node (Node (Node Leaf 1 Leaf) 3 (Node Leaf 4 Leaf)) 5
          (Node (Node Leaf 7 Leaf) 13 (Node Leaf 14 (Node Leaf 17 Leaf)))
t6 = Node (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) 4
          (Node (Node Leaf 5 Leaf) 6 (Node Leaf 7 (Node Leaf 8 Leaf)))
-}


t123 = Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)
t567 = Node 6 (Node 5 Nil Nil) (Node 7 Nil Nil)



-- testing closest with different distanced nodes
t7 = Node 8 (Node 5  (Node 1 Nil Nil)  (Node 6 Nil Nil))
            (Node 20 (Node 19 Nil Nil) (Node 21 Nil Nil))

tdup = Node 8 (Node 8 (Node 7 Nil Nil) Nil) (Node 10 (Node 9 Nil Nil) Nil)


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





mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Nil = Nil
mapTree f (Node n left right) = Node (f n) (mapTree f left) (mapTree f right)


preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]



--- alternative solution
flattenPre :: BinaryTree a -> [a] -> [a]
flattenPre Leaf accList = accList
flattenPre (Node left a right) accList
    = a : flattenPre left (flattenPre right accList)

flattenIn :: BinaryTree a -> [a] -> [a]
flattenIn Leaf accList = accList
flattenIn (Node left a right) accList
    = flattenIn left (a : (flattenIn right accList))

flattenPost :: BinaryTree a -> [a] -> [a]
flattenPost Leaf accList = accList
flattenPost (Node left a right) accList
    = flattenPost left (flattenPost right (a : accList))

preorder' :: BinaryTree a -> [a]
preorder' tree = flattenPre tree []

inorder' :: BinaryTree a -> [a]
inorder' tree = flattenIn tree []

postorder' :: BinaryTree a -> [a]
postorder' tree = flattenPost tree []



--- Foldr for binary tree ---------------------------------------------------------------
--- todo to mull over - had help on this one.
--- note does preorder traversal (see example foldtree)
-- a : flattenPre left (flattenPre right accList)
foldrPreorder :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrPreorder _ acc Leaf = acc
foldrPreorder f acc (Node left x right) = accRight
    where --- foldTree f (foldTree f (f x acc) left) right
    accNode = f x acc
    accLeft = foldrPreorder f accNode left
    accRight = foldrPreorder f accLeft right

printSumPreorderRight :: Show a => a -> String -> String
printSumPreorderRight x y = "(" ++ show x ++ "+" ++ y ++ ")"

------------------
--- note does preorder traversal backwards (see example left foldtree)
-- a : flattenPre left (flattenPre right accList)
foldPreorderLeft :: (b -> a -> b) -> b -> BinaryTree a -> b
foldPreorderLeft _ acc Leaf = acc
foldPreorderLeft f acc (Node left x right) = accRight
    where
    accNode = f acc x
    accLeft = foldPreorderLeft f accNode left
    accRight = foldPreorderLeft f accLeft right

printPreorderLeft :: Show a => String -> a -> String
printPreorderLeft x y = "(" ++ x ++ "+" ++ show y ++ ")"

------------------
-- flattenIn left (a : (flattenIn right accList))
foldInorder :: (b -> a -> b -> b) -> b -> BinaryTree a -> b
foldInorder _ acc Leaf = acc
foldInorder f acc (Node left x right)
    = f (foldInorder f acc left) x (foldInorder f acc right)

printSumInorder :: Show a => String -> a -> String -> String
printSumInorder x y z = "(" ++ x ++ "+" ++ show y ++ "+" ++ z ++ ")"

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
exampleFoldTreePRE_R = foldrPreorder printSumPreorderRight "0" t3
-- note preorder foldl (backwards)
exampleFoldTreePRE_L = foldPreorderLeft printPreorderLeft "0" t3
-- note inorder
exampleFoldTreeIN = foldInorder printSumInorder "0" t3
--exampleFoldTreePOST =


--- Rewrite map using foldtree ------------------------------------------------------------

--- idea for later: construct a binary search tree using postorder/inorder/preorder
-- traversal.
-- note returns mapped list in swirly preorder traversal.
mapFoldTreeToList :: (a -> b) -> BinaryTree a -> [b]
mapFoldTreeToList f tree = foldrPreorder ((:) . f) [] tree

-- HELP HELP HELP TODO map fold but return binary tree.
{-mapFoldTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapFoldTree f tree = foldrPreorder mk Leaf tree
    where mk a Leaf = Node Leaf (f a) Leaf
          mk a (Node l x r) = -}







---------------------------------------------------------------------------------------

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Node n left right) = Node (f n) (fmap f left) (fmap f right)



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


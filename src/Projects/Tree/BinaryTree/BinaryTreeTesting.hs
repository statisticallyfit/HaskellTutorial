module BinaryTreeTesting where

import Test.QuickCheck -- (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Control.Monad hiding (join)
import Data.Maybe
import Data.List (findIndex, elemIndex, sort)

import BinaryTreeOps ---- then load that file with this file in cmd line ghci

--- TESTING ------------------------------------------------------------------------------

{-
NOTE
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = do
        x <- arbitrary
        l <- arbitrary
        r <- arbitrary
        frequency [(1, return Nil), -}
{-(1, return (Leaf x)),-}{-

                   (1, return (Node l x r))]
-}

{-
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = frequency
        [(1, return Nil),
         (4, liftM3 Node arbitrary arbitrary arbitrary)]
-}

--- NOTE faster tree declaration
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized arbTree
arbTree 0 = return Nil --liftM NilT arbitrary
arbTree n = frequency [(1, return Nil),
                       (4, liftM3 Node arbitrary (arbTree (n `div` 2))
                                                 (arbTree (n `div` 2)) )]

instance Eq a => EqProp (Tree a) where (=-=) = eq




-- note size before should be same as size after.
-- HELP doesn't work because of Nil t2.
testJoin :: Tree Int -> Tree Int -> Bool
testJoin t1 t2 = (size t1 + size t2) == (size (join t1 t2))

testInsert :: Int -> Tree Int -> Bool
testInsert x tree = (size tree + 1) == (size (insert x tree))


testMap :: Tree Int -> Bool
testMap tree = (collapse $ mapTree (+3) tree) == (map (+3) (collapse tree))

testSize :: Tree Int -> Bool
testSize tree = size tree == (length $ collapse tree)

-- test inserts and deletes!

testOccurs :: Int -> Tree Int -> Bool
testOccurs n tree = (elem n (collapse tree)) == (occurs n tree)

testInorderEqualsCollapse :: Tree Int -> Bool
testInorderEqualsCollapse tree = collapse tree == inorder tree


testFold :: Int -> Tree Int -> Bool
testFold acc tree = (preFoldr (+) acc tree) == (foldr (+) acc flatTree)
    where flatTree = collapse tree


testPreFoldlIsFoldl :: Int -> Tree Int -> Bool
testPreFoldlIsFoldl acc tree = (preFoldl f acc tree) == (foldl f acc tree)
    where f = \acc y -> acc - y

main = do
    quickCheck testMap
    quickCheck testSize
    quickCheck testOccurs
    quickCheck testInorderEqualsCollapse
    quickCheck testFold
    quickCheckWith stdArgs {maxSuccess = 1000} testPreFoldlIsFoldl

    let trigger = undefined :: Tree (Int, Int, [Int])
    quickBatch (traversable trigger)




{-
---------------------------------------------------------
sampleTree :: Tree Integer
sampleTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder = if preorder testTree' == [2,1,3]
               then putStrLn "Preorder is fine!"
               else putStrLn "Bad news bears."

testInorder =
    if inorder testTree' == [1,2,3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder =
    if postorder testTree' == [1,3,2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears."
-}

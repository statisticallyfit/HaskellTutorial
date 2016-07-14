module Foldable where


import Data.Foldable
import Data.Monoid


{-
class Foldable (t :: * -> *) where
    note fold combines (squashes) elements inside monoidal structure.
    fold    :: Monoid m => t m      -> m
    note foldMap first maps each element of structure to a monoid and then squashes
    using mappend instance of the Monoid given.
    foldMap :: Monoid m => (a -> m) -> t a -> m
-}

sumFold = fold [1,2,3,4,5 :: Sum Integer]
prodFold = fold [1,2,3,4,5 :: Product Integer]

sumFoldMap  = foldMap Sum [1,2,3,4,5]
prodFoldMap = foldMap Product [1,2,3,4,5]
allFoldMap  = foldMap All [True, False, True]
anyFoldMap  = foldMap Any [True, False, True]
firstFoldMap = foldMap First [Just 1, Nothing, Just 5] -- these get the last Just n
lastFoldMap = foldMap Last [Just 1, Nothing, Just 5]


-- note foldMap can take a non-monoid function and use it to map to end Monoid instance.
ex1 = foldMap (*5) [1,2,3 :: Product Integer] -- 5 * 10 * 15 = 750
ex2 = foldMap (*5) [1,2,3 :: Sum Integer] -- 5 + 10 + 15 == 30


-- note if what you are folding over has 1 value then declaring Monoid instance won't
-- change final behavior
ex3 = foldMap (*5) (Just 100) :: Product Integer -- 500 
ex4 = foldMap (*5) (Just 5) :: Sum Integer -- 25
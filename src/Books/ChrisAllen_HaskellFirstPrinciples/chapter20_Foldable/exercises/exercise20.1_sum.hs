

import Data.Foldable
import Data.Monoid


{-
instance Foldable Option where
    foldr _ z None = z
    foldr f z (Some x) = f x z

    foldl _ z None = z
    foldl f z (Some x) = f z x

    foldMap _ None = mempty
    foldMap f (Some x) = f x
-}
{-

sumFold = fold [1,2,3,4,5 :: Sum Integer]
prodFold = fold [1,2,3,4,5 :: Product Integer]

sumFoldMap  = foldMap Sum [1,2,3,4,5]
prodFoldMap = foldMap Product [1,2,3,4,5]
allFoldMap  = foldMap All [True, False, True]
anyFoldMap  = foldMap Any [True, False, True]
firstFoldMap = foldMap First [Just 1, Nothing, Just 5] -- these get the last Just n
lastFoldMap = foldMap Last [Just 1, Nothing, Just 5]
-}


sumFoldr :: (Foldable t, Num a) => t a -> a
sumFoldr xs = foldr (+) 0 xs


sumFoldl :: (Foldable t, Num a) => t a -> a
sumFoldl xs = foldl (+) 0 xs

sumFoldMap :: (Foldable t, Num a) => t a -> Sum a
sumFoldMap xs = foldMap Sum xs


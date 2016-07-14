

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



sumFoldr :: (Foldable t, Num a) => t a -> a
sumFoldr xs = foldr (+) 0 xs


sumFoldl :: (Foldable t, Num a) => t a -> a
sumFoldl xs = foldl (+) 0 xs

sumFoldMap :: (Foldable t, Num a) => t a -> a
sumFoldMap = getSum . foldMap Sum



main :: IO()
main = do
    print $ sumFoldMap [1,2,3] == 6
    print $ sumFoldMap (Just 3) == 3
    print $ sumFoldMap Nothing == 0
    print $ fmap sumFoldMap (Just [1,2,3]) == Just 6
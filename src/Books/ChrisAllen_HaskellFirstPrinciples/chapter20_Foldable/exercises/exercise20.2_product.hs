
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


productFoldr :: (Foldable t, Num a) => t a -> a
productFoldr xs = foldr (*) 1 xs

productFoldl :: (Foldable t, Num a) => t a -> a
productFoldl xs = foldl (*) 1 xs

productFoldMap :: (Foldable t, Num a) => t a -> a
productFoldMap = getProduct . foldMap Product



main :: IO()
main = do
    print $ productFoldMap [1,2,3,4] == 24
    print $ productFoldMap (Just 3) == 3
    print $ productFoldMap Nothing == 1
    print $ fmap productFoldMap (Just [1,2,3,4]) == Just 24
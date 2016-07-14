
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


-- help how to do foldr and foldl way?
{-elemFoldr :: (Foldable t, Eq a) => a -> t a -> Bool
elemFoldr x xs = foldr (== x) [] xs-}

-- note maps a function (e == x) then applies Any to the list of bools.
elemFoldMap:: (Foldable t, Eq a) => a -> t a -> Bool
elemFoldMap x xs = getAny $ foldMap (\e -> Any $ e == x) xs


main :: IO()
main = do
    print $ elemFoldMap 2 (Just 3)
    print $ elemFoldMap True (Right True)
    print $ fmap (elemFoldMap 2) [Nothing]
    print $ fmap (elemFoldMap 3) [Right 1, Right 2, Left 4, Right 3]
    print $ fmap (elemFoldMap 1) (Just [1,2,3]) -- Just True
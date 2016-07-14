
import Data.Foldable
import Data.Monoid
import Data.Maybe

{-
instance Foldable Option where
    foldr _ z None = z
    foldr f z (Some x) = f x z

    foldl _ z None = z
    foldl f z (Some x) = f z x

    foldMap _ None = mempty
    foldMap f (Some x) = f x
-}


newtype Min a = Min {getMin :: Maybe a}

instance Ord a => Monoid (Min a) where
    mempty = Min Nothing
    (Min (Just x)) `mappend` Min Nothing  = Min (Just x)
    Min Nothing    `mappend` Min (Just y) = Min (Just y)
    Min (Just x)   `mappend` Min (Just y)
        | x <= y = Min (Just x)
        | otherwise = Min (Just y)



minimumFoldMap :: (Foldable t, Ord a) => t a -> a
minimumFoldMap xs = fromJust $ getMin $ foldMap (\x -> Min {getMin = Just x}) xs



m1 = minimumFoldMap [6,5,12,5,5,5,435] == 5
m2 = minimumFoldMap "julie" == 'e'
m3 = fmap minimumFoldMap  (Just "julie") == (Just 'e')
m4 = fmap minimumFoldMap [Just 'j', Just 'u', Just 'l'] == "jul"
m5 = fmap minimumFoldMap [Just 4, Just 3, Nothing] -- exception at Nothing
m6 = fmap minimumFoldMap [Just 4, Just 5, Just 10] == [4,5,10]
--m6 = minimumFoldMap (Left 3)


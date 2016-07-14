
import Data.Foldable
import Data.Monoid
import Data.Maybe


newtype Max a = Max {getMax :: Maybe a}

instance Ord a => Monoid (Max a) where
    mempty = Max Nothing
    (Max (Just x)) `mappend` Max Nothing  = Max (Just x)
    Max Nothing    `mappend` Max (Just y) = Max (Just y)
    Max (Just x)   `mappend` Max (Just y)
        | x > y = Max (Just x)
        | otherwise = Max (Just y)


-- help how to declare these two for the general foldable? not only lists?
maximumFoldr :: Ord a => {-(Foldable t, Ord a) => t a-}[a] -> a
maximumFoldr [] = undefined
maximumFoldr (x:xs) = foldr (\a acc -> if a > acc then a else acc) x xs

maximumFoldl :: Ord a => [a] -> a
maximumFoldl [] = undefined
maximumFoldl (x:xs) = foldl (\acc b -> if b > acc then b else acc) x xs

maximumFoldMap :: (Foldable t, Ord a) => t a -> a
maximumFoldMap xs = fromJust $ getMax $ foldMap (\x -> Max {getMax = Just x}) xs



m1 = maximumFoldMap [1,2,4,67,9,43,7,8] == 67
m2 = maximumFoldMap "julie" == 'u'
m3 = maximumFoldMap (Just "julie") == "julie"
m4 = fmap maximumFoldMap (Just "julie") == Just 'u'
m5 = maximumFoldMap (Just [1,2,3]) == [1,2,3]
m6 = fmap maximumFoldMap (Just [1,2,4]) == Just 4
m7 = fmap (fmap maximumFoldMap) [Just [4,5,1], Just [3,1], Just [10,8]]
            == [Just 5, Just 3, Just 10]

allTrue = and $ fmap getAll $ fmap All [m1, m2, m3, m4, m5, m6, m7]
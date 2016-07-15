
import Data.Foldable
import Data.Monoid



data Four a b = Four a b b b deriving (Eq, Show)


instance Foldable (Four a) where
-- help how to write foldr and foldl?
    foldr f z (Four a b c d) = (d `f` (c `f` (b `f` z)))
    foldl f z (Four a b c d) = (((z `f` b) `f` c) `f` d)
    foldMap f (Four a b c d) = f b <> f c <> f d




main :: IO()
main = do
    print $ foldMap Sum (Four "a" 2 4 5)
    print $ foldMap Sum (Four 1 2 8 9)
    print $ foldr (+) 1 (Four 1 2 3 4)
    print $ fmap (foldr (-) (-8)) [Four "a" 3 3 2, Four "d" 1 6 5, Four "f" 7 1 (-9)]
    print $ fmap (foldl (-) (-8)) [Four "a" 3 3 2, Four "d" 1 6 5, Four "f" 7 1 (-9)]
    print $ foldMap Sum (Four 1 2 4 5)
    print $ foldMap Any (Four 1 False True True)
    print $ foldMap First (Four 0 (Nothing) (Just 100) (Just 5))
    print $ foldMap Last (Four 0 (Just 44) (Just 100) (Just 23))
    print $ foldMap Last (Four 0 (Just 23) (Just 100) Nothing)


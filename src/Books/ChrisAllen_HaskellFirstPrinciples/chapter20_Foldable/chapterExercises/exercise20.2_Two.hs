

import Data.Foldable
import Data.Monoid



data Two a b = Two a b deriving (Eq, Show)


instance Foldable (Two a) where
    foldr f z (Two x y) = f y z
    foldl f z (Two x y) = f z y
    foldMap f (Two x y) = f y




main :: IO()
main = do
    print $ foldr (-) (-8) (Two 1 (-2))
    print $ foldl (-) (-8) (Two 1 (-2))
    print $ foldMap Sum (Two 10 100)
    print $ fmap (foldMap Product) [Two 4 5, Two 9 8, Two 6 5]
    print $ foldMap Any (Two "a" False)
    print $ fmap (foldr (-) 5) [Two 1 2, Two 9 8]
    print $ foldMap First (Two (Just 8) (Just 7))

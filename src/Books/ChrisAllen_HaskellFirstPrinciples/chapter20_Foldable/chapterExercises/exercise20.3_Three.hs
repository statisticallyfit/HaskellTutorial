
import Data.Foldable
import Data.Monoid



data Three a b c = Three a b c deriving (Eq, Show)


instance Foldable (Three a b) where
    foldr f z (Three a b c) = f c z
    foldl f z (Three a b c) = f z c
    foldMap f (Three a b c) = f c



main :: IO()
main = do
    print $ fmap (foldr (-) (-8)) [Three "a" "b" 3, Three "d" "e" 6, Three "f" "g" (-9)]
    print $ fmap (foldl (-) (-8)) [Three 1 2 3, Three 4 5 6, Three 7 8 (-9)]
    print $ foldMap Sum (Three 1 2 3)
    print $ foldMap Any (Three 1 2 True)
    print $ foldMap First (Three 0 0 (Just 100))

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



data Three' a b = Three' a b b deriving (Eq, Show)


instance Foldable (Three' a) where
    foldr f z (Three' a b c) = c `f` (b `f` z)
    foldl f z (Three' a b c) = (z `f` b) `f` c
    foldMap f (Three' a b c) = (f b) <> (f c)




main :: IO()
main = do
    print $ foldMap Sum (Three' "a" 2 4)
    print $ foldMap Sum (Three' 1 2 8)
    print $ fmap (foldr (-) (-8)) [Three' "a" 3 3, Three' "d" 1 6, Three' "f" 7 (-9)]
    print $ fmap (foldl (-) (-8)) [Three' "a" 3 3, Three' "d" 1 6, Three' "f" 7 (-9)]
    print $ foldMap Sum (Three' 1 2 4)
    print $ foldMap Any (Three' 1 False True)
    print $ foldMap First (Three' 0 (Nothing) (Just 100))
    print $ foldMap Last (Three' 0 (Just 44) (Just 100))
    print $ foldMap Last (Three' 0 (Just 23) (Just 100))

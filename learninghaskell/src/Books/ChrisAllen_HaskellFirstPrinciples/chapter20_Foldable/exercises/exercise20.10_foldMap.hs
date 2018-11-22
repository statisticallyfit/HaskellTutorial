



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


foldMapAsFoldr :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMapAsFoldr f xs = foldr (\x acc -> f x <> acc) mempty xs


main :: IO()
main = do
    print $ foldMap Sum [1 .. 10]
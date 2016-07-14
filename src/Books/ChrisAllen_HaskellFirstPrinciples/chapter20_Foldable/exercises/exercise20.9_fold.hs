

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

foldAsFoldMap :: (Foldable t, Monoid m)=> t m -> m
foldAsFoldMap xs = foldMap id xs



main :: IO()
main = do
    print $ foldAsFoldMap [1,2,3,4,5 :: Sum Integer]
    print $ foldAsFoldMap [1,2,3,4,5 :: Product Integer]
    print $ foldAsFoldMap ["hi", " there"]



import Data.Foldable
import Data.Monoid



{-
NOTE example
foldMap :: Monoid m => (a -> m) -> t a -> m
foldMap _ None = mempty
foldMap f (Some x) = f x
-}


nullFoldr :: Foldable t => t a -> Bool
nullFoldr xs = foldr (\_ _ -> False) True xs

nullFoldl :: Foldable t => t a -> Bool
nullFoldl xs = foldl (\_ _ -> False) True xs

-- help todo todo understand how const $ ALl False works better...
nullFoldMap :: (Foldable t) => t a -> Bool
nullFoldMap = getAll . foldMap (const $ All False)

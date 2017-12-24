

import Data.Foldable
import Data.Monoid



toListFoldr :: Foldable t => t a -> [a]
toListFoldr xs = foldr (:) [] xs

toListFoldl :: Foldable t => t a -> [a]
toListFoldl xs = foldl (flip (:)) [] xs

-- foldMap :: Monoid m => (a -> m) -> t a -> m
toListFoldMap :: Foldable t => t a -> [a]
toListFoldMap xs = foldMap (\x -> [x]) xs


main :: IO()
main = do
    print $ toListFoldr (Just 1)
    print $ toListFoldr (Just [1..10])
    print $ toListFoldMap [Just 1, Just 2, Just 3]
    print $ map toListFoldr [Just 1, Just 2, Just 3]
    print $ concatMap toListFoldr [Just 1, Just 2, Just 3]
    print $ concatMap toListFoldr [Just 1, Just 2, Nothing]
    --print $ toListFoldr Nothing -- help understand
    print $ toListFoldr (1,2) -- help why doesn't toList (1,2,3) work?
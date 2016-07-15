

import Data.Foldable
import Data.Monoid



-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

filterF :: (Applicative t, Foldable t, Monoid (t a)) => (a -> Bool) -> t a -> t a
filterF p xs = foldMap (\x -> if p x then pure x else mempty) xs


main = do
    print $ filterF odd [1..10]

{-
help help help todo why don't these work?
main :: IO()
main = do
    print $ filterF (const False) (1, Sum 1) :: [Sum Int]
    print $ filterF (const True)  (1, Sum 1) :: [Sum Int]
    print $ filterF (const True)  (1, Sum 1) :: Maybe (Sum Int)
    print $ filterF (const False) (1, Sum 1) :: Maybe (Sum Int)
-}

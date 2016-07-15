


import Data.Foldable
import Data.Monoid




data Constant a b = Constant a deriving (Eq, Show)


instance Foldable (Constant a) where
    foldMap _ _ = mempty
    -- note below does not work because when we write Foldable (Constant a)
    -- we take away the only arg we have to work on in Constant.
    -- And we must write Foldable (Constant a) because :k Constant is * -> * -> *
    -- while :k FOldable is * -> * and so Constant must take an (a) at the top.
    {-foldr f z (Constant x) = f x z
    foldl f z (Constant x) = f z x
    foldMap f (Constant x) = f x-}



main :: IO()
main = do
    print $ foldl (+) 1 (Constant 2)
    print $ foldMap Sum (Constant 10)
    print $ fmap ({-getProduct . -}foldMap Product) [Constant 10, Constant 111]
    print $ fmap (foldr (-) 5) [Constant 1, Constant 2] -- returns seed for both
    print $ foldMap Any (Constant 1)
    print $ foldMap First (Constant 1)
    print $ foldMap (\x -> Any True) (Constant 1) -- help meaning?



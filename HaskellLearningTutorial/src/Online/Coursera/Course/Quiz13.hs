module Online.Coursera.Course.Quiz13 where

{-
NOTE:

foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-}


-- choose all correct definitions of foldl in terms of foldr
-- HELP meaning?
foldl_1        :: (b -> a -> b) -> b -> [a] -> b
foldl_1 f a bs = foldr (\b -> \g -> (\a -> g (f a b))) id bs a

-- HELP
foldl_2   :: (b -> a -> b) -> b -> [a] -> b
foldl_2 f = flip $ foldr (\a b g -> b (f g a)) id

main = do
    print $ foldl_1 (+) 0 [1,2,3,4,5]

import Prelude hiding (map, filter)
import Data.Foldable

--map f xs = foldr (f) xs
-- map defined  in terms of foldr

-- note: Structures that can be replaced by foldr
--f [] = v
--f (x:xs) = x (OP) f xs
-- is
-- foldr (OP) v xs

-- note: Definition of foldr
--foldr :: (a -> b -> b) -> b -> [a] -> b
--foldr f v [] = v
--foldr f v (x:xs) = f x (foldr f v xs)


map'      :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

map''          :: (a -> b) -> [a] -> [b]
map'' _ []     = []
map'' f (x:xs) = f x : map'' f xs



map   :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> f x:xs) []
-- note: the xs is what the foldr will recurse on.
-- help: todo: understand better

notmap f = foldr (\x xs -> f x ++ xs) []

map3 f = foldl (\xs x -> xs ++ [f x]) []
-- help: todo: understand better


filter   :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x xs -> if p x then x:xs else xs) []
----------- foldr ------------- (OP) -------------- seed

main = do
    print $ map' even [2,4,6,8,9]
    print $ map'' odd [1,3,5,2]
    print $ map even [2,4,5,3]
    --print $ notmap even [1,2,3,4,5,6,7,8,9]
    putStrLn "map3: ";
    print $ map3 even [1,2,3,4,5,6,7,8,9]
    print $ filter even [2,4,6,5,3,1]
    print $ filter odd [2,4,1,3,(-5),7,8,9]
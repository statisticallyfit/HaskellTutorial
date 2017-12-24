import Prelude

evens                    :: [Int] -> [Int]
evens []                 = []
evens (x:xs) | even x    = x : evens xs
             | otherwise = evens xs

(◦) :: (b -> c) -> (a -> b) -> (a -> c)
f ◦ g = \x -> f (g x)

----------------------------------------------------------------------------------
higherOrder        :: (a -> b) -> (a -> Bool) -> [a] -> [b]
higherOrder f p = map f ◦ filter p
--[f x | x <- xs, p x]
-- or can be written like this
-- map f (filter p xs) if given xs in the function args




main = print ""
 --print $ higherOrder (sum even [1..10])
-- help: todo: how to provide arguments? gives errors
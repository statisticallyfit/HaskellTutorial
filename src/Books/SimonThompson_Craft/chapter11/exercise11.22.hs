


{-
NOTE

:t ($ 3)
($ 3) :: Num a => (a -> b) -> b

-- means it expects a function on the left to apply to the argument 3.
example: id $ 3
-}



-- note from exercise 11.13
-- note given list of functions (fs) and arg (x) apply all functions to (x)
mapFuns1 :: [a -> b] -> a -> [b]
mapFuns1 fs = \x -> ( ($ x) `map` fs)

{-
NOTE use flip because
(\x -> (map ($ x)  ))  ::  a -> [a -> b] -> [b]

because the argument x is given first and we expect a list of functions after ($ x)
-}
mapFuns2 :: [a -> b] -> a -> [b]
mapFuns2 = \fs x -> ( ($ x) `map` fs)



mapFuns3 :: [a -> b] -> a -> [b]
mapFuns3 = flip (\x -> ( ($ x) `map` {-fs here-}))



mapFuns4 :: [a -> b] -> a -> [b]
mapFuns4 = \fs x -> zipWith ($) fs (repeat x)


mapFuns5 :: [a -> b] -> a -> [b]
mapFuns5 fs = \x -> (flip map) fs ($ x)
-- note flip map so it takes the list first, and the function ($ x) second.


mapFuns6 :: [a -> b] -> a -> [b]
mapFuns6 = \fs x -> (\f -> f x) `map` fs
-- note the function is (\f -> f x) that map takes first and second is list fs.


eval = mapFuns4 [(+1),(*8),(+2)] 2
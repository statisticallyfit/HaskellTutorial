

-- []

-- help why gives error?
--(<@>) :: [(a -> b)] -> [a] -> [b]
--(<@>) (f:fs) xs = (fmap f xs) : (fs <@> xs)


main = do
    print $ (pure :: (a -> [] a)) 4
    print $ ((<*>) :: [] (a -> b) -> [] a -> [] b) [(+1), (*3)] [1,1,1]
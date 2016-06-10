

mult :: Int -> Int -> Int
mult x y
    | x == 0 || y == 0 = 0
    | otherwise        = x * y


fact :: Int -> Int
fact n
    | n == 0 = 1
    | otherwise = n * fact (n-1)

{-
uncover
main = do
    print $ fact 4; print $ fact 5; print $ fact 6
    print $ mult 0 10
    print $ mult 0 (fact (-2))
    -- print $ mult (fact (-2)) 0 -- note stack overflow


-}





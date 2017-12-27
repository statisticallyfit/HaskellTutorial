
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> (x * y * z)))
-- note: mult x y z= \x -> (\y -> (\z -> (x * y * z))) -- wrong

main = print (mult 8 9 4)
sumFunc :: (Int -> Int) -> Int -> Int
sumFunc f n
    | n == 0 = f 0
    | n > 0  = sumFunc f (n-1) + f n


regions :: Int -> Int
regions n = (sumFunc (\x -> x) n) + 1 -- can use id function or lambda


reg :: Int -> Int
reg n
    | n == 0 = 1
    | n > 0  = reg (n-1) + n


-- help: todo: exercise 4.12

main = do
    print $ regions 8 --37

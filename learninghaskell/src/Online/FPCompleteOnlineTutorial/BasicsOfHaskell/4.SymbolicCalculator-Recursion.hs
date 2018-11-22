-- Three steps:
-- 1. lexical analysis - string is converted to tokens
-- 2. parsing - building expression tree
-- 3. evaluation

-- the return function is a monadic function - every monad has it.
-- it turns whatever value it is given into a monadic value. So it can turn
--- the () into IO()



-- Exercise 1 ----------------------------------------------------------------------
loop :: Int -> IO()
loop n = do
    if n <= 10
    then do
        putStr ", ";  putStr (show (n^2))
        loop (n+1)
    else do
        putStrLn ""
        return ()

-- help: todo: why dosn't this work?
--[putStrLn (x^2) | x <- [n..10]]


{-}main :: IO()
main = loop 1
-}
-- Exercise 2,3 ----------------------------------------------------------------------
fact :: Integer -> Integer
fact n = if n > 0 then n*fact(n-1) else 1

--main = print (fact 20)


-- Exercise 3 ----------------------------------------------------------------------

fib :: Integer -> Integer
fib n = if n > 2 then fib(n-2) + fib(n-1) else 1


main = print $ fib 20

-- Exercise 4 ----------------------------------------------------------------------




{-}main :: IO()
main = do
    line <- getLine
    putStrLn line
    main -- tail recursion, infinite loop

-}
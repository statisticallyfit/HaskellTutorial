
factorial :: [Integer]
factorial = map fact [0..]

fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact n = n * fact (n-1)


------------------------------

fibonacci :: [Integer]
fibonacci = [0,1] ++ map fib [1..]

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
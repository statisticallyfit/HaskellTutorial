
-- precondition: given n, find its divisors
divisors :: Integer -> [Integer]
divisors n = [d | d <- [1..n], n `mod` d == 0]

isPrime   :: Integer -> Bool
isPrime n = length (divisors n) == 2
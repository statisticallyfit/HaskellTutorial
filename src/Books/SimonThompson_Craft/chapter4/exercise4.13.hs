import Prelude hiding (gcd)
-- Find gcd algorithm

gcd :: Int -> Int -> Int
gcd a b
    | a == 0 || b == 0 = error "Not allowed zeroes"
    | a < 0 || b < 0   = error "Not allowed negative numbers"
    | a < b            = gcd b a
    | a `mod` b == 0   = b
    | otherwise        = gcd b (a `mod` b)



main = do
    print $ gcd 64 40; print $ gcd 40 64
    print $ gcd 32 24
    print $ gcd 30 105
    --print $ gcd (-2) 10
    --print $ gcd 0 10
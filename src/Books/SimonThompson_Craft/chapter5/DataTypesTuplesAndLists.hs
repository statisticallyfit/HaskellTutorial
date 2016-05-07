import Prelude


-- 2. Tuple Types

type ShopItem = (String, Int)
--("Salt: 1kg", 139) :: ShopItem

type Basket = [ShopItem]
--[("Salt: 1kg", 139), ("Plain crisps", 25), ("Plain crisps", 25)]


-- use tuple to return compound result
minAndMax :: Integer -> Integer -> (Integer, Integer)
minAndMax x y
    | x >= y    = (y, x)
    | otherwise = (x, y)



fib   :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fibStep        :: (Integer, Integer) -> (Integer, Integer)
fibStep (u, v) = (v, u+v)

fibPair        :: Integer -> (Integer, Integer)
fibPair n
    | n == 0    = (0, 1)
    | otherwise = fibStep (fibPair (n-1))

fastFib   :: Integer -> Integer
fastFib n = fst (fibPair n)
-- help:: todo: the below doesn't work - why?
--fastFib n = fst . (fibPair n)


main = do
    print $ fibStep (0, 1); print $ fibStep (2, 3); print $ fibStep (9, 14)
    print $ fibPair 5; print $ fibPair 8
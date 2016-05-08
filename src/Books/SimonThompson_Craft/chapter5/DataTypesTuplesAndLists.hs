import Prelude


-- 5.2. Tuple Types

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






-- 5.3 Algebraic data types -------------------------------------------------------

-- (1) enumerated types
data Move = Rock | Paper | Scissors -- note: this is a nullary constructor
            deriving (Eq, Show)


score :: Move -> Move -> Integer
score Rock Rock         = 0
score Paper Paper       = 0
score Scissors Scissors = 0
score Rock Paper        = -1
score Paper Rock        = 1
score Rock Scissors     = 1
score Scissors Rock     = -1
score Paper Scissors    = -1
score Scissors Paper    = 1

-- (2) product types
data People = Person Name Age -- note: this is a binary constructor
              deriving (Eq, Show)
type Name = String
type Age = Int

showPerson :: People -> String
showPerson (Person str n) = str ++ " -- " ++ show n


-- (3) tuples and data types
--type People = (Name, Age)


main = do
    print $ fibStep (0, 1); print $ fibStep (2, 3)----- tuple tupes
    print $ fibStep (9, 14)
    print $ fibPair 5; print $ fibPair 8
    ---------------------------------------------------- algebraic types
    print $ score Rock Scissors
    print $ Person "Electric Aunt Jemima" 77
    print $ Person "Ronnie" 14
    print $ showPerson (Person "Electric Aunt Jemima" 77)
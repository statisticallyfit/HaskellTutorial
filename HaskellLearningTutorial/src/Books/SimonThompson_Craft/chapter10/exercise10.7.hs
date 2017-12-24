
--1
minResult :: (Integer -> Integer) -> [Integer] -> Integer
minResult f ns = minimum (map f ns)



-- 2
areEqualResult :: (Integer -> Integer) -> [Integer] -> Bool
areEqualResult f ns = areEqual (map f ns)

areEqual          :: Eq a => [a] -> Bool
areEqual []       = True
areEqual (a:b:cs) = a == b && areEqual cs



-- 3
areGreaterZeroResult :: (Integer -> Integer) -> [Integer] -> Bool
areGreaterZeroResult f ns = results == (filter (> 0) results)
    where results = map f ns


-- 4
areIncreasingResults :: (Integer -> Integer) -> [Integer] -> Bool
areIncreasingResults f ns = areInc (map f ns)

areInc          :: [Integer] -> Bool
areInc []       = False
areInc [a]      = True
areInc [a,b]    = a <= b
areInc (a:b:cs) = a <= b && areInc (b:cs)
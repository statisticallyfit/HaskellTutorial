

onSeperateLines :: [String] -> String
onSeperateLines stringList = concat [word ++ "\n" | word <- stringList]


pushRight :: String -> Int -> String
pushRight xs lineLength = [' ' | x <- [1 .. lineLength - n]] ++ xs
                          where n = length xs

{-

fib :: Integer -> Integer
fib n
    | n == 0    = 0
    | n == 1    = 1
    | otherwise = fib (n-1) + fib (n-2)
-}


fibList :: [Integer]
fibList = 0 : 1 : [x + y | (x,y) <- zip fibList (tail fibList)]

fib   :: Int -> Integer
fib n = head (drop n fibList)


fibTable :: Int -> String
fibTable n = onSeperateLines $ ["n" ++ pushRight "fib n" 10] ++
             [(show i) ++ (pushRight (show (fib i)) 10) | i <- [0..n]]
-- HELP how to do so that it is leftright justified? so there is always a gap?
-- past 100, the n and fib n cols meld together.

main = do
    putStr $ fibTable 20
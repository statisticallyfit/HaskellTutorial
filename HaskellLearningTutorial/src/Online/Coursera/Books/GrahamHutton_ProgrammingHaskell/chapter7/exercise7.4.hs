
-- help this did not work
--decToInt'        :: [Int] -> String
--decToInt' []     = ""
--decToInt' (x:xs) = read (show x ++ decToInt' xs)

decToInt :: [Int] -> Int
decToInt = foldl (\x y -> 10*x + y) 0
-- help: utodo: nderstand this notation more
-- note: todo: true that y is the one that decToInt will recurse over?
-- note: todo: can you recognize it because it is the one not on the lambda, like x?


main = do
    print $ decToInt [2,3,4,5]
    print $ decToInt [1,2,3,4,5,6,0]
    print $ decToInt [0,1,2,0]
    print $ decToInt []
    print $ decToInt [0,0,0,0]
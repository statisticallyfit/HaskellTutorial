-- had help here and understand answer now.

getInt :: IO Integer
getInt = do line <- getLine
            return (read line :: Integer)


addNums          :: Integer -> Integer -> IO Integer
addNums 0 carry  = return carry
addNums n carry  = do num <- getInt
                      addNums (n-1) (carry + num)



readNIntsAndSum :: IO ()
readNIntsAndSum = do putStr "Enter count: "
                     count <- getInt
                     do putStrLn "Enter the numbers: "
                        sum <- addNums count 0
                        putStrLn ("The sum: " ++ show sum)


main = readNIntsAndSum
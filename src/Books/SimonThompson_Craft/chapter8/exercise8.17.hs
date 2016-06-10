

getInt :: IO Integer
getInt = do line <- getLine
            let int = (read line :: Integer)
            return int


sumUntilInputZero :: IO ()
sumUntilInputZero = do putStr "intPrompt >  "
                       summed <- adder 0
                       putStrLn ("Sum = " ++ (show summed))

    where adder :: Integer -> IO Integer
          adder carry = do input <- getInt
                           if input /= 0
                           then (
                                do putStr "intPrompt >  "
                                   adder (input + carry)
                                )
                           else (return carry)

main = sumUntilInputZero
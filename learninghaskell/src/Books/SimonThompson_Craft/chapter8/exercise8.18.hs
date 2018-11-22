getInt :: IO Integer
getInt = do line <- getLine
            return (read line :: Integer)



-- precondition: the list y:ys is sorted ascendingly. Duplicates are allowed.
-- postcondition: the result list is sorted ascendingly and duplicates are removed.
insert           :: Integer -> [Integer] -> [Integer]
insert x []      = [x]
insert x (y:ys)
    | y >= x     = x : y : ys
    | otherwise  = y : insert x ys


sortUntilInputZero :: IO ()
sortUntilInputZero = do putStrLn "You must enter numbers to be sorted."
                        sortedNums <- sorter []
                        putStrLn ("The sorted numbers are: " ++ (show sortedNums))

    where sorter :: [Integer] -> IO [Integer]
          sorter carrySorted = do putStr "num >  "
                                  n <- getInt
                                  if n /= 0
                                  then sorter (insert n carrySorted)
                                  else (return carrySorted)

main = sortUntilInputZero


readWrite :: IO()
readWrite = do getLine
               putStrLn "one line read"

readEcho :: IO()
readEcho = do line <- getLine
              putStrLn ("line read: " ++ line)


------------------------------------------------------

-- Note adding a sequence of integers

getInt :: IO Int
getInt = fmap read getLine -- note fmap lifts read over the IO String and right into String


sumInts :: Int -> IO Int
sumInts s = do n <- getInt
               if n == 0
               then return s
               else sumInts (s + n)

-- HELP todo understand why this doesn't just return 0 when you type in 0. Why does
-- it still return the sum ? 
sumInts2 :: IO Int
sumInts2 = do n <- getInt
              if n == 0
              then return 0
              else (do m <- sumInts2
                       return (n+m))

sumAcc s [] = s
sumAcc s (n:ns)
    = if n == 0
      then s
      else sumAcc (s + n) ns

sumInteract :: IO()
sumInteract = do putStrLn "Enter integers one per line, "
                 putStrLn "these will be summed until zero is entered."
                 sum <- sumInts 0
                 putStr "The sum is: "
                 print sum
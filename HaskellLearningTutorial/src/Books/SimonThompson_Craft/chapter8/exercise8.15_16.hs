import Data.Char (isAlphaNum, toLower)


clean :: String -> String
clean []           = []
clean (x:xs)
    | isAlphaNum x = toLower x : clean xs
    | otherwise    = clean xs

-- precondition - takes a cleaned version of the string word
isPalin    :: String -> Bool
isPalin [] = True
isPalin [x] = True
isPalin word
    | firstLetter == lastLetter = isPalin middle
    | otherwise                 = False
    where firstLetter = head word
          lastLetter = last word
          middle = init $ tail word


isPalindrome :: String -> Bool
isPalindrome word = isPalin $ clean word

{-

readPotentialPalindrome :: IO Bool
readPotentialPalindrome = do line <- getLine
                             return (isPalindrome line)
-}


palindromeChecker :: IO ()
palindromeChecker = do putStr "Write a palindrome: "
                       line <- getLine
                       if line /= ""
                       then (
                            do let bool = isPalindrome line
                               putStrLn ("Palindrome? : " ++ (show bool))
                               palindromeChecker
                            )
                       else return ()


main = palindromeChecker -- and then input this at GHC: "Madam, I'm Adam!"
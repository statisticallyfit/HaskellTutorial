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


readPotentialPalindrome :: IO Bool
readPotentialPalindrome = do line <- getLine
                             return (isPalindrome line)



main = do
    readPotentialPalindrome -- and then input this at GHC: "Madam, I'm Adam!"
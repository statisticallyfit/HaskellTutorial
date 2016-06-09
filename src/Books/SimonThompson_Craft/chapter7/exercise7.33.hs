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




main = do
    print $ clean "Doc, note. I dissent. A fast never prevents a fatness. I diet on Cod."
    print $ isPalindrome "Doc, note. I dissent. A fast never prevents a fatness. I diet on Cod."
    putStrLn ""
    print $ clean "Cigar? Toss it in a can, it is so tragic."
    print $ isPalindrome "Cigar? Toss it in a can, it is so tragic."
    putStrLn ""
    print $ clean "Stressed? No tips ? Spit on desserts."
    print $ isPalindrome "Stressed? No tips ? Spit on desserts."
    putStrLn ""
    print $ clean "Madam, I'm Adam"
    print $ isPalindrome "Madam, I'm Adam"
    putStrLn ""
    print $ clean "racecar"
    print $ isPalindrome "racecar"
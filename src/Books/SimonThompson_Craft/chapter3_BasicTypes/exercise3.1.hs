
{-Give another version of the definition of 'exclusive or'
which works informally thus: 'exclusive or of x and y will be
True if either x is True and y is False. or vice versa'.
-}

exOr' :: Bool -> Bool -> Bool
exOr' x y
    | x == y    = False
    | otherwise = True


solution :: Bool -> Bool -> Bool
solution x y =
    x == True && y == False
    ||
    x == False && y == True

main = do
    print (exOr' True True)
    print (solution True True)
    putStrLn ""
    ----------------------------
    print (exOr' True False)
    print (solution True False)
    putStrLn ""
    -----------------------------
    print (exOr' False True)
    print (solution False True)
    putStrLn ""
    -----------------------------
    print (exOr' False False)
    print (solution False False)
    putStrLn  ""
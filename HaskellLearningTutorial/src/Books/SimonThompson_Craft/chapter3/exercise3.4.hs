{-Give two different definitions of the nAnd function
nAnd :: Bool -> Bool -> Bool
which returns the result True except when both its arguments are True.
Give a diagram illustrating one of your definitions.
-}

nAnd :: Bool -> Bool -> Bool
nAnd x y
    | x == True && y == True = False
    | otherwise              = True


nAnd' :: Bool -> Bool -> Bool
nAnd' True True = False
nAnd' a b       = True



main = do
    print (nAnd True True)
    print (nAnd True False)
    print (nAnd False True)
    print (nAnd False False)

xor :: Bool -> Bool -> Bool
xor True True = True
xor True False = True
xor False True = True
xor False False = False

xor' :: Bool -> Bool -> Bool
xor' b c
    | b == c && b == False = False
    | otherwise            = True


xor5 :: Bool -> Bool -> Bool
xor5 b c
    | b == c    = b
    | otherwise = True


xor'' :: Bool -> Bool -> Bool
xor'' False False = False
xor'' _ _         = True


xor''' :: Bool -> Bool -> Bool
xor''' a b = if a == False && b == False then False else True


xor4 :: Bool -> Bool -> Bool
xor4 False b = b
xor4 True _  = True





main = do
    print (xor False False, xor True False,
        xor True True, xor False True)
    print (xor' False False, xor' True False,
        xor True True, xor False True)
    print (xor'' False False, xor'' True False,
        xor'' True True, xor'' False True)
    print (xor''' False False, xor''' True False,
        xor''' True True, xor''' False True)
    print (xor4 False False, xor4 True False,
            xor4 True True, xor4 False True)
    print (xor5 False False, xor5 True False,
            xor5 True True, xor5 False True)

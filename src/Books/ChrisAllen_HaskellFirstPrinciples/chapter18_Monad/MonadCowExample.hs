
data Cow = Cow {
    name   :: String,
    age    :: Int,
    weight :: Int
} deriving (Eq, Show)

noEmpty     :: String -> Maybe String
noEmpy ""   = Nothing
noEmpty str = Just str

noNegative               :: Int -> Maybe Int
noNegative n | n >= 0    = Just n
             | otherwise = Nothing

weightCheck   :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
        in if n == "Bess" && w > 499
        then Nothing
        else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
    case noEmpty name' of
    Nothing    -> Nothing
    Just nammy ->
        case noNegative age' of
            Nothing   -> Nothing
            Just agey ->
                case noNegative weight' of
                    Nothing      -> Nothing
                    Just weighty ->
                        weightCheck (Cow nammy agey weighty)


-- note: cleaning up function with monad - no need to repeat anymore like above.
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
    nammy   <- noEmpty name'
    agey    <- noNegative age'
    weighty <- noNegative weight'
    weightCheck (Cow nammy agey weighty)

-- note: this is how the above code looks like, desugared:
mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' = do
    noEmpty name' >>=
        \nammy ->
        noNegative age' >>=
            \agey ->
            noNegative weight' >>=
                \weighty ->
                weightCheck (Cow nammy agey weighty)


{-
example: passing some arguments:

mkSphericalCow'' "Bess" 5 499
    noEmpty "Bess" >>=
        \nammy ->
        noNegative 5 >>=
            \agey ->
            noNegative 499 >>=
                \weighty ->
                weightCheck (Cow nammy agey weight)



mkSphericalCow'' "Bess" 5 499
    noEmpty "Bess" >>=
        \"Bess" ->              equals: Just "Bess" but with >>= it is "Bess"
        noNegative 5 >>=
            \agey ->
            noNegative 499 >>=
                \weighty ->
                weightCheck (Cow nammy agey weight)



mkSphericalCow'' "Bess" 5 499
    noEmpty "Bess" >>=
        \"Bess" ->
        noNegative 5 >>=
            \5 ->               equals: Just 5 but with >>= it is (5)
            noNegative 499 >>=
                \weighty ->
                weightCheck (Cow nammy agey weight)


mkSphericalCow'' "Bess" 5 499
    noEmpty "Bess" >>=
        \"Bess" ->
        noNegative 5 >>=
            \5 ->
            noNegative 499 >>=
                \499 ->          equals: Just 499 but with >>= it is (499)
                weightCheck (Cow nammy agey weight)


-- note: but if had a "" for name?
mkSphericalCow'' "" 5 499
    noEmpty "" >>=    equals: Nothing >>= _ so the entire computation drops.
        \nammy ->
        noNegative 5 >>=
            \agey ->
            noNegative 499 >>=
                \weighty ->
                weightCheck (Cow nammy agey weight)

summary: entire computation drops any moment that any function in the Maybe Monad
actions produce Nothing, because of:

instance Monad Maybe where
    return x = Just x
    (Just x) >>= k     = k x    key so run the entire computation over x
    Nothing >>=  _     Nothing  key so the entire computation is dropped
-}



main = do
    print $ mkSphericalCow "Bess" 5 499
    print $ mkSphericalCow "Bess" 5 500

    print $ mkSphericalCow' "Bess" 5 499
    print $ mkSphericalCow' "Bess" 5 500
    putStrLn ""
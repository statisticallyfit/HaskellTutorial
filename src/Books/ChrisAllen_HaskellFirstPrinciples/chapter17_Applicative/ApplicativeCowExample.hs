import Control.Applicative


data Cow = Cow {
    name   :: String,
    age    :: Int,
    weight :: Int
} deriving (Eq, Show)


noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing


-- Validating to get rid of empty strings, negative numbers
cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
    case noEmpty name' of
        Nothing -> Nothing
        Just nammy ->
            case noNegative age' of
                Nothing -> Nothing
                Just agey ->
                    case noNegative weight' of
                        Nothing -> Nothing
                        Just weighty -> Just (Cow nammy agey weighty)


-- improving cowstring
cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name' age' weight' =
    Cow <$> noEmpty name'
        <*> noNegative age'
        <*> noNegative weight'


cowFromString'' :: String -> Int -> Int -> Maybe Cow
cowFromString'' name' age' weight' =
    liftA3 Cow (noEmpty name')
               (noNegative age')
               (noNegative weight')


cow1 = Cow <$> noEmpty "Bess"  -- Equals :t is Maybe (Int -> Int -> Cow)
                               -- because we took up the first String arg
cow2 = cow1 <*> noNegative 1   -- equals :t is Maybe (Int -> Cow)
cow3 = cow2 <*> noNegative 2   -- equals :t is Maybe Cow

cow1' = liftA3 Cow             -- equals :t is Applicative f => f String -> f Int
                               -- -> f Int -> f Cow which is
                               -- Maybe String -> Maybe Int -> Maybe Int -> Maybe Cow
cow2' = cow1' (noEmpty "blah") -- equals: :t Maybe Int -> Maybe Int -> Maybe Cow
cow3' = cow2' (noNegative 1)   -- equals Maybe Int -> Maybe Cow
cow4' = cow3' (noNegative 2)   -- equals Maybe Cow


--- 1
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False



--- 2

-- >> maybe 0 (+1) Nothing    --> 0
-- >> maybe 55 (+1) (Just 13) --> 14
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee defaultVal f (Just x) = f x
mayybee defaultVal _ Nothing = defaultVal



--- 3

-- >> fromMaybe 0 Nothing  ---> 0
-- >> fromMaybe 0 (Just 1) ---> 1
fromMaybe :: a -> Maybe a -> a
fromMaybe defaultVal (Just x) = x
fromMaybe defaultVal Nothing = defaultVal


fromJust :: Maybe a -> a
fromJust (Just x) = x


--- 4

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just $ head xs


--- 5
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]


--- 6
catMaybes :: [Maybe a] -> [a]
catMaybes xs = concatMap filterJusts xs
    where filterJusts = (\m -> if isJust m then [fromJust m] else [])


--- 6
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms
    | allJust = Just $ map fromJust ms
    | otherwise = Nothing
    where allJust = and $ map isJust ms
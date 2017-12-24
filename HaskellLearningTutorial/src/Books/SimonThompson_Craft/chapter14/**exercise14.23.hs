import Data.List


-- note type: transmitting an error
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe g Nothing = Nothing
mapMaybe g (Just x) = Just (g x)

-- note type: trapping an error
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing = n
maybe n f (Just x) = f x


-- HELP how to define it with above two functions and
-- just normally like below?
-- exercise 23 ---------------------------------------
-- note given the indexes n and m from xs list, find the nth and mth items + sum them.
-- should return 0 if either elements are not one of the indices of the list.
process :: [Int] -> Int -> Int -> Int
process xs n m = case (elem )
{-
process xs n m = case (findIndex n xs) of
                        Nothing -> 0
                        Just nElem ->
                            case (findIndex m xs) of
                                Nothing -> 0
                                Just mElem -> mElem + nElem-}

import Data.Char (isDigit)

-- remove the first element which does not have property p.
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ []   = []
filterFirst p (x:xs)
    | p x == False = xs
    | otherwise    = x : filterFirst p xs


-- exercise 19 --------------------------------------------------------------------
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast _ [] = []
filterLast p xs = reverse $ filterFirst p (reverse xs)


filterLast' :: (a -> Bool) -> [a] -> [a]
filterLast' _ [] = []
filterLast' p xs
    | p (last xs) == False = init xs
    | otherwise            = (filterLast' p (init xs)) ++ [last xs]
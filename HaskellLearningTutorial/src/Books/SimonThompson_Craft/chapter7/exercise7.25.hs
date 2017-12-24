import Data.List (elem, elemIndex)
import Data.Char (toLower)

-- note is sub a sublist of list?
-- "ship" and "Fish and Chips" --- TRUE
-- "hippies" and "Fish and Chips" -- FALSE

isSublist :: Eq a => [a] -> [a] -> Bool
isSublist [] _ = True
--isSublist [] list = True
isSublist _ [] = False
isSublist sub list = subFirstElem `elem` list &&
                     isSublist (tail sub) (remove subFirstElem list)
                     where subFirstElem = head sub
                          -- list' = fmap toLower list

-- note doesn't always work
isSublist' :: Eq a => [a] -> [a] -> Bool
isSublist' [] _ = True
isSublist' _ [] = False
isSublist' (x:xs) (y:ys)
    | x == y    = isSublist xs ys
    | otherwise = isSublist (x:xs) ys



-- note removes the first occurrence of an element in the given list
remove :: Eq a => a -> [a] -> [a]
remove x xs = fst splitList ++ (tail $ snd splitList)
              where (Just xPos) = elemIndex x xs
                    splitList = splitAt xPos xs

----------------------------------------------------------------------------------------

isSubsequence :: Eq a => [a] -> [a] -> Bool
isSubsequence _ []     = False
isSubsequence sub list = (sub == (take n list)) || -- note - OR all it needs is one TRUE
                         isSubsequence sub (tail list)
                         where n = length sub



main = do
    print $ isSublist "ship" "Fish and CHIPS" -- FALSE
    print $ isSublist "siF" "Fish and Chips" -- TRUE
    print $ isSublist "hipp" "Fish and Chips" -- FALSE
    print $ isSublist [1,2,3] [10,4,2,1,5,3] -- TRUE
    print $ isSublist "dna" "Fish and Chips"
    print $ isSublist "pahC" "Fish and Chips"
    putStrLn "" -----------------------------------------------------------------------
    print $ isSublist' "dna" "Fish and Chips"
    print $ isSublist' "pahC" "Fish and Chips"
    print $ isSublist' "ballet" "Sunrise lights the lovely turrets by day"
    print $ isSublist' [1,2,3,10] [10,4,2,1,5,3]
    -- note this would be TRUE if 10 didn't start list2. TRUE if the items are reversed
    -- next to each other like "2,1" but not if the reversed item comes much before
    -- like "10,4,2,1". But if this is true for the numbers, then why do all aother
    --- much-reversed tests work with strings?
    putStrLn "" ------------------------------------------------------------------------
    print $ isSubsequence "Chip" "Fish and Chips"
    print $ isSubsequence "ship" "Fish and Chips"
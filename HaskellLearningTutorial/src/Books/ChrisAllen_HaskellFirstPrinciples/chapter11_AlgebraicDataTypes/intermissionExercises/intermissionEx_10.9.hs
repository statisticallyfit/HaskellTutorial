module Jammin where

import Data.List


data Fruit = Apple | Blackberry | Peach | Plum deriving (Eq, Show, Ord)

data JamJars = Jam {fruit :: Fruit, jars :: Int} deriving (Eq, Show, Ord)

--- 3
-- cardinality of JamJars == cardinality of Fruit (4) times cardinality of Int


--- 5
row1 = Jam {fruit = Plum, jars = 8}
row2 = Jam Apple 13
row3 = Jam Peach 3
row4 = Jam Blackberry 5
row5 = Jam Plum 5
row6 = Jam Apple 10
row7 = Jam Blackberry 6
row8 = Jam Blackberry 2
row9 = Jam Peach 14
row10 = Jam Peach 4
allJamJars = [row1, row2, row3, row4, row5, row6, row7, row8, row9, row10]

rowJars :: [JamJars] -> [Int]
rowJars = map jars


--- 6
jarsCount :: [JamJars] -> Int
jarsCount {-jjs-} = sum . rowJars {-jjs-}


--- 7
-- note gets the maximum jar num
mostRow :: [JamJars] -> JamJars
mostRow [] = undefined
mostRow  (jj:jjs) = foldr maxJar jj jjs
    where maxJar jj1 accJJ2
            | jars jj1 > jars accJJ2 = jj1
            | otherwise = accJJ2

mostRow' :: [JamJars] -> JamJars
mostRow' = maximumBy (\j1 j2 -> compare (jars j1) (jars j2))

-- need a seed so need first element of list.
{-mostRow'' :: [JamJars] -> JamJars
mostRow'' = foldr (\j1 accJ2 -> compare (jars j1) (jars accJ2)) -}




--- 9
compareKind (Jam k _) (Jam k' _) = compare k k'

sortJams :: [JamJars] -> [JamJars]
sortJams jjs = sortBy compareKind jjs

{-
-- note fold doesn't work here since it returns a single value, not a list.
sortJams' :: [JamJars] -> [JamJars]
sortJams' [] = []
sortJams' (j:js) = foldl f j js
    where f acc j
            | compareKind j acc == LT = j
            | otherwise = acc
-}





--- 10
groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy (\j1 j2 -> fruit j1 == fruit j2)  . sortJams
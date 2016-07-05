module Jammin where

import Data.List 


data Fruit = Peach | Plum | Apple | Blackberry deriving (Eq, Show, Ord)

data JamJars = Jam {fruit :: Fruit, jars :: Int} deriving (Eq, Show, Ord)

--- 3
-- cardinality of JamJars == cardinality of Fruit (4) times cardinality of Int


--- 5
row1 = Jam {fruit = Plum, jars = 5}
row2 = Jam Apple 10
row3 = Jam Peach 3
row4 = Jam Blackberry 2
row5 = Jam Plum 8
row6 = Jam Apple 13
allJamJars = [row1, row2, row3, row4, row5, row6]

rowJars :: [JamJars] -> [Int]
rowJars = map jars


--- 6
getAllJars :: [JamJars] -> Int
getAllJars {-jjs-} = sum . rowJars {-jjs-}


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

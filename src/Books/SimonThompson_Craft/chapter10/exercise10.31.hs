import Data.List

type Picture = [[Char]]


flipV :: Picture -> Picture
flipV pic = map reverse pic


-- note: rotates clockwise 90 degrees
-- got help here.
rotate :: Picture -> Picture
--rotate [] = [] -- help is this needed?
rotate ([]:_) = [] -- help can the _ mean there is nothing after the []? Or
-- are there cases that there are contents after the []?
rotate pic = reverse (map head pic) : (rotate (map tail pic))


rotate' :: Picture -> Picture
rotate' = (map reverse) . transpose

{-rotate :: Picture -> Picture
rotate pic = flipV [ [xs !! i | xs <- pic] | i <- [0 .. rowLength - 1]]
                        where rowLength = length (pic !! 0)-}



draw :: Picture -> IO()
draw pic = putStr $ onSeperateLines pic
   where onSeperateLines pic = [letter | picLine <- pic, letter <- picLine ++ "\n"]


p1 = ["##########", "######....", "###.......", "##........", "#........."]
p2 = ["##########", "###....###", "###....###", "###....###", "##########"]
p  = ["...####...", "...#..#...", "...####...", "...#......", "...#......"]
b  = ["...####...", "...#...#..", "...####...", "...#...#..", "...####..."]
l  = ["..#.......", "..#.......", "..#.......", "..#.......", "..######.."]
n  = ["..#....#..", "..#.#..#..", "..#..#.#..", "..#...##..", "..#....#.."]


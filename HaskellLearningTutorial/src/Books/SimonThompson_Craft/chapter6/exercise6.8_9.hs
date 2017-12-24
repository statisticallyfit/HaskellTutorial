type Picture = [[Char]]


flipV :: Picture -> Picture
flipV pic = [reverse line | line <- pic]


{-
NOTE:
1. take an i = 0
2. for i = 0 take a line from the pic and then take the 0th element.
3. then take the next line and take the 0th element.
4. then the next line ...
5. once finished all the lines in the pic, go back to step 1 with i = 1, then
take i = 1 from all lines in pic, then i = 2 from all lines in pic...
6. once done all i's then flipV.
-}
rotateClockwise90 :: Picture -> Picture
rotateClockwise90 pic = flipV [ [xs !! i | xs <- pic] | i <- [0 .. rowLength - 1]]
                        where rowLength = length (pic !! 0)


rotateCounterclockwise90 :: Picture -> Picture
rotateCounterclockwise90 pic = rotateClockwise90 $ rotateClockwise90 $ rotateClockwise90 pic


draw :: Picture -> IO()
draw pic = putStr $ onSeperateLines pic
           where onSeperateLines pic = [letter | picLine <- pic, letter <- picLine ++ "\n"]


p1 = ["##########", "######....", "###.......", "##........", "#........."]
p2 = ["##########", "###....###", "###....###", "###....###", "##########"]
p  = ["...####...", "...#..#...", "...####...", "...#......", "...#......"]
b  = ["...####...", "...#...#..", "...####...", "...#...#..", "...####..."]
l  = ["..#.......", "..#.......", "..#.......", "..#.......", "..######.."]
n  = ["..#....#..", "..#.#..#..", "..#..#.#..", "..#...##..", "..#....#.."]




main = do
    draw p
    draw $ rotateClockwise90 p
    draw $ rotateCounterclockwise90 p
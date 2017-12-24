import Data.List


type Picture = [[Char]]
type Row = String
type Position = (Int, Int)

-- note has reverse effect of makePicture




pictureToRep :: Picture -> (Int, Int, [Position])
pictureToRep pic = (l, w, positions)
    where w = length pic
          l = length (pic !! 0)
          (xPos, yPos) = (getXBlacks pic, getYBlacks pic)
          match xss ys = concat [ [(x,y) | x <- xs]  | (xs, y) <- zip xss ys]
          positions = match xPos yPos


getYBlacks :: Picture -> [Int]
getYBlacks pic = yPos
    where bools = map containsBlack pic
          yPos = findIndices (== True) bools
          containsBlack row = or $ map (== '#') row


-- note gets position of all the '#' in a Row
getXBlacks :: Picture -> [[Int]]
getXBlacks pic = filter (/= []) (map get pic)
    where get row = getPos row 0 []


-- row = line string, ex: "#..#.#."
-- (int,int) = tuple that counts positions
-- [(int,int)] = list of tuples that hold positions of each '#' sign.
getPos :: Row -> Int -> [Int] -> [Int]
getPos [] _ xs  = xs
getPos (c:cs) x xs
    | c == '#'  = getPos cs (x+1) (xs ++ [x])
    | otherwise = getPos cs (x+1) xs






draw :: Picture -> IO()
draw pic = putStr $ onSeperateLines pic
 where onSeperateLines pic = concat $ map (++ "\n") pic


p1 = ["##########", "######....", "###.......", "##........", "#........."]
p2 = ["##########", "###....###", "###....###", "###....###", "##########"]
p = ["...####...", "...#..#...", "...####...", "...#......", "...#......"]
b = ["...####...", "...#...#..", "...####...", "...#...#..", "...####..."]
l = ["..#.......", "..#.......", "..#.......", "..#.......", "..######.."]
n = ["..#....#..", "..#.#..#..", "..#..#.#..", "..#...##..", "..#....#.."]
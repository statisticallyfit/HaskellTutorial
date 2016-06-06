
-- 6.6 POSITIONED PICTURES  ----------------------------------------------------------
type Picture = [[Char]]
type Position = (Int, Int)
type Image = (Picture, Position)





raggedHorsePic = [".......##..........##...",
                  ".....##..#.......##..#..",
                  "...##.....#....##.....#.",
                  "..#.......#...#.......#.",
                  "..#...#...#...#...#...#.",
                  "..#...###.#...#...###.#.",
                  ".#....#..##..#....#..##.",
                  "..#...#.......#...#.....",
                  "...#...#.......#...#....",
                  "....#..#........#..#....",
                  ".....#.#.........#.#....",
                  "......##..........##....",
                  ".......##....",
                  ".....##..#...",
                  "...##.....#..",
                  "..#.......#..",
                  "..#...#...#..",
                  "..#...###.#..",
                  ".#....#..##.",
                  "..#...#.....",
                  "...#...#.....",
                  "....#..#....",
                  ".....#.#....",
                  "......##...."]


p1 = ["##########", "######....", "###.......", "##........", "#........."]
p2 = ["##########", "###....###", "###....###", "###....###", "##########"]
p = ["...####...", "...#..#...", "...####...", "...#......", "...#......"]
b = ["...####...", "...#...#..", "...####...", "...#...#..", "...####..."]
l = ["..#.......", "..#.......", "..#.......", "..#.......", "..######.."]
n = ["..#....#..", "..#.#..#..", "..#..#.#..", "..#...##..", "..#....#.."]





padPicture     :: Picture -> Picture
padPicture pic = [line ++ (replicate (maxWidth - (length line)) '.') | line <- pic]
                 where maxWidth = maximum [length line | line <- pic]


above :: Picture -> Picture -> Picture
above = (++)

beside :: Picture -> Picture -> Picture
beside picL picR = [lineL ++ lineR | (lineL, lineR) <- zip picL picR]


flipH :: Picture -> Picture
flipH = reverse


flipV :: Picture -> Picture
flipV pic = [reverse line | line <- pic]


-- note: rotate clockwise 90 degrees
rotate :: Picture -> Picture
rotate pic = flipV [ [xs !! i | xs <- pic] | i <- [0 .. rowLength - 1]]
             where rowLength = length (pic !! 0)

-- note rotate counterclockwise 90 degrees
rotateCounter :: Picture -> Picture
rotateCounter pic = rotate $ rotate $ rotate pic



draw :: Picture -> IO()
draw pic = putStr $ onSeperateLines pic
 where
    onSeperateLines pic = [letter | picLine <- pic, letter <- picLine ++ "\n"]

----------------------------------------------------------------------------------------


-- exercise 29
makeImage :: Picture -> Position -> Image
makeImage pic pos = (pic, pos)

-- exercise 30
changePosition :: Image -> Position -> Image
changePosition (pic, _) newPos = (pic, newPos)


-- exercise 31
moveImage :: Image -> Int -> Int -> Image
moveImage (pic, (x,y)) xMove yMove = (pic, (x + xMove, y + yMove))


-- exercise 32
-- moves cursor to a given position
{-
HELP how to do this?
goto :: Position -> IO()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")



drawImageLine          :: String -> Position -> IO()
drawImageLine line pos = do goto pos
                            putStr line

drawImage :: Image -> IO()
drawImage (pic, pos) = concat [drawImageLine line pos | line <- pic]


main = do
    drawImage $ makeImage (p, (1,2))

-}



-- exercise 33 -- assuming images stay in same positions throughout transformations -----
flipHNaive :: Image -> Image
flipHNaive (pic, _) = flipH pic

flipVNaive :: Image -> Image
flipVNaive (pic, _) = flipV pic

-- note: rotate clockwise 90 degrees
rotateNaive :: Image -> Image
rotateNaive (pic, _) = rotate pic

-- note rotate counterclockwise 90 degrees
rotateCounter :: Image -> Image
rotateCounter (pic, _) = rotateNaive $ rotateNaive $ rotateNaive pic




-- exercise 34 -- assuming geometrical view, don't stay in same position ---------------
height     :: Picture -> Int
height pic = length pic

-- precondition assume picture is padded
width     :: Picture -> Int
width pic = length (pic !! 0) -- length of any row
----------------------------------------------------------------------------------------

-- precondition picture doesn't have to be padded
-- precondition assume position is top left corner of image
flipHGeo :: Image -> Image
flipHGeo (pic, (x,y)) = (flipHNaive pic, (x + w, y))
                        where pic' = padPicture pic
                              w = width pic'
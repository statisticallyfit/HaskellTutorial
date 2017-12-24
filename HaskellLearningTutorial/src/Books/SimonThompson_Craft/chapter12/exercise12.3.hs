
type Picture = [[Char]]
type Row = String
type Position = (Int, Int)


-- exercise 3


makePicture :: Int -> Int -> [(Int, Int)] -> Picture
makePicture l w blacksPos = finalPic
    where box = replicate w (replicate l '.')
          newRow px py = replaceAt px '#' (getRow py box)
          newBox px py = putRow py (newRow px py) box
          boxList = [makePicRow box pos | pos <- blacksPos]
          finalPic = zipSuperimposeAll box boxList



zipSuperimposeAll :: Picture -> [Picture] -> Picture
zipSuperimposeAll acc [] = acc
zipSuperimposeAll acc (p:ps) = zipSuperimposeAll acc' ps
    where acc' = zipWith superimposeRow acc p


-- note given a tuple of position, it changes the row to hold that position
makePicRow :: Picture -> Position -> Picture
makePicRow pic (x,y) = newBox
    where newRow = replicate x '.' ++ "#"
                   ++ (replicate (length (pic !! 0) - (x+1)) '.')
          newBox = putRow y newRow pic


getRow :: Int -> Picture -> Row
getRow y box = box !! y

-- superimposes a row with one at index y and puts the new one at y in box list.
putRow :: Int -> Row -> Picture -> Picture
putRow y givenRow box = replaceAt y combinedRow box
    where boxRow = getRow y box
          combinedRow = superimposeRow boxRow givenRow


superimposeRow :: Row -> Row -> Row
superimposeRow r1 r2 = zipWith superimposeChar r1 r2
    where superimposeChar a b = if a == b && a == '.' then '.' else '#'


replaceAt :: Int -> a -> [a] -> [a]
replaceAt pos x xs = fst spLine ++ [x] ++ (tail $ snd spLine)
    where spLine = splitAt pos xs



-- note puts spaces between "#." in a line.
spacify     :: String -> String
spacify line = trim spaced
    where spaced = concat $ map (\c -> [c] ++ " ") line
          trim s = reverse $ dropWhile (== ' ') (reverse s)







draw :: Picture -> IO()
draw pic = putStr $ onSeperateLines pic
 where onSeperateLines pic = concat $ map (++ "\n") pic


p1 = ["##########", "######....", "###.......", "##........", "#........."]
p2 = ["##########", "###....###", "###....###", "###....###", "##########"]
p = ["...####...", "...#..#...", "...####...", "...#......", "...#......"]
b = ["...####...", "...#...#..", "...####...", "...#...#..", "...####..."]
l = ["..#.......", "..#.......", "..#.......", "..#.......", "..######.."]
n = ["..#....#..", "..#.#..#..", "..#..#.#..", "..#...##..", "..#....#.."]
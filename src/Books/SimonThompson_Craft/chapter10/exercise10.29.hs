
type Picture = [[Char]]



invertChar :: Char -> Char
invertChar ch = if ch == '.' then '#' else '.'

-- note now apply inversion to all lines in the picture
invertColour :: Picture -> Picture
invertColour pic = map (map invertChar) pic  -- or (map . map) invert pic
-- map invertLine pic



draw :: Picture -> IO()
draw pic = putStr $ onSeperateLines pic
 where
    onSeperateLines pic = [letter | picLine <- pic, letter <- picLine ++ "\n"]




p1 = ["##########", "######....", "###.......", "##........", "#........."]
p2 = ["##########", "###....###", "###....###", "###....###", "##########"]
p = ["...####...", "...#..#...", "...####...", "...#......", "...#......"]
b = ["...####...", "...#...#..", "...####...", "...#...#..", "...####..."]
l = ["..#.......", "..#.......", "..#.......", "..#.......", "..######.."]
n = ["..#....#..", "..#.#..#..", "..#..#.#..", "..#...##..", "..#....#.."]
type Picture = [[Char]]


superimposeChar :: Char -> Char -> Char
superimposeChar '.' '.' = '.'
superimposeChar _ _ = '#'


superimposeLine :: String -> String -> String
superimposeLine [] [] = []
superimposeLine (a:as) [] = superimposeChar a '.' : superimposeLine as []
superimposeLine [] (b:bs) = superimposeChar '.' b : superimposeLine [] bs
superimposeLine (a:as) (b:bs) = superimposeChar a b : superimposeLine as bs


superimpose :: Picture -> Picture -> Picture
superimpose p1 p2 = zipWith superimposeLine p1 p2



draw :: Picture -> IO()
draw pic = putStr $ onSeperateLines pic
 where
    onSeperateLines pic = [letter | picLine <- pic, letter <- picLine ++ "\n"]




p1 = ["##########", "######....", "###.......", "##........", "#........."]
p2 = ["##########", "###....###", "###....###", "###....###", "##########"]
p = ["...####...", "...#..#...", "...####...", "...#......", "...#......"]
smallP = [".##.", ".##.", ".#.."]
b = ["...####...", "...#...#..", "...####...", "...#...#..", "...####..."]
l = ["..#.......", "..#.......", "..#.......", "..#.......", "..######.."]
n = ["..#....#..", "..#.#..#..", "..#..#.#..", "..#...##..", "..#....#.."]
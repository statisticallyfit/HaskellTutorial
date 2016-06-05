type Picture = [[Char]]


draw :: Picture -> IO()
draw pic = putStr $ onSeperateLines pic
           where onSeperateLines pic = [letter | picLine <- pic, letter <- picLine ++ "\n"]



-- had he.lp here.
scaleLineH        :: String -> Int -> String
scaleLineH line n = concat [replicate n ch | ch <- line]


scaleH :: Picture -> Int -> Picture
scaleH pic n = [scaleLineH line n | line <- pic]


scaleV :: Picture -> Int -> Picture
scaleV pic n = concat [replicate n line | line <- pic]


scale :: Picture -> Int -> Picture
scale pic n = scaleV (scaleH pic n) n


p1 = ["##########", "######....", "###.......", "##........", "#........."]
p2 = ["##########", "###....###", "###....###", "###....###", "##########"]
p3 = ["#.#", "..#"]
p  = ["...####...", "...#..#...", "...####...", "...#......", "...#......"]
b  = ["...####...", "...#...#..", "...####...", "...#...#..", "...####..."]
l  = ["..#.......", "..#.......", "..#.......", "..#.......", "..######.."]
n  = ["..#....#..", "..#.#..#..", "..#..#.#..", "..#...##..", "..#....#.."]


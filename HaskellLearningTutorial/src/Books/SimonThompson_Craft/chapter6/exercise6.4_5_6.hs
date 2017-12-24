type Picture = [[Char]]


superimposeChar :: Char -> Char -> Char
superimposeChar '.' '.' = '.'
superimposeChar _ _ = '#'


superimposeLine :: String -> String -> String
superimposeLine l1 l2 = [superimposeChar c1 c2 | (c1, c2) <- zip l1 l2]


superimpose :: Picture -> Picture -> Picture
superimpose p1 p2 = [superimposeLine l1 l2 | (l1, l2) <- zip p1 p2{-l1 <- p1, l2 <- p2-}]
-- note the commented out part is basically a foreach - best to use zip. So otherwise
-- it would be for each l1, do the same l2, ...


{-
onSeperateLines :: Picture -> String
onSeperateLines stringList = concat [word ++ "\n" | word <- stringList]
-}

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
    print $ superimposeChar '.' '.'
    print $ superimposeChar '.' '#'
    print $ superimposeChar '#' '.'
    print $ superimposeChar '#' '#'
    putStrLn "" --------------------------
    print $ superimposeLine ".##." ".#.#"
    putStrLn "" --------------------------
    draw $ superimpose n p1
    draw $ superimpose p1 n
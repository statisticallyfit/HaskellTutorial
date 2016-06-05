type Picture = [[Char]]


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


padPicture     :: Picture -> Picture
padPicture pic = [line ++ (replicate (maxWidth - (length line)) '.') | line <- pic]
                 where maxWidth = maximum [length line | line <- pic]
                       --pad = maxWidth - (length line)



draw :: Picture -> IO()
draw pic = putStr $ onSeperateLines pic
           where
                onSeperateLines pic = [letter | picLine <- pic, letter <- picLine ++ "\n"]



main = do
    draw $ raggedHorsePic
    putStrLn ""
    draw $ padPicture raggedHorsePic
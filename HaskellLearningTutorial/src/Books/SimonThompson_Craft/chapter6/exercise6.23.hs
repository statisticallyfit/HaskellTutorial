-- RUN LENGTH ENCODING ----------------------------------------------------------------
type PictureRLE = [[(Integer, Char)]]
-- HELP why if I change Integer to Int, why does drawRLE give error?


raggedHorsePicRLE = [[(7,'.'), (2,'#'), (10,'.'), (2,'#'), (3,'.')],
                     [(5,'.'), (2,'#'), (2,'.'), (1,'#'), (7,'.'), (2,'#'), (2,'.'), (1,'#'), (2,'.')],
                     [(3,'.'), (2,'#'), (5,'.'), (1,'#'), (4,'.'), (2,'#'), (5,'.'), (1,'#'), (1,'.')],
                     [(2,'.'), (1,'#'), (7,'.'), (1,'#'), (3,'.'), (1,'#'), (7,'.'), (1,'#'), (1,'.')],
                     [(2,'.'), (1,'#'), (3,'.'), (1,'#'), (3,'.'), (1,'#'), (3,'.'), (1,'#'), (3,'.'), (1,'#'), (3,'.'), (1,'#')],
                     [(2,'.'), (1,'#'), (3,'.'), (3,'#'), (1,'.'), (1,'#'), (3,'.'), (1,'#'), (3,'.'), (3,'#'), (1,'.'), (1,'#')],
                     [(1,'.'), (1,'#'), (4,'.'), (1,'#'), (2,'.'), (2,'#'), (2,'.'), (1,'#'), (4,'.'), (1,'#'), (2,'.'), (2,'#')],
                     [(2,'.'), (1,'#'), (3,'.'), (1,'#'), (7,'.'), (1,'#'), (3,'.'), (1,'#'), (5,'.')],
                     [(3,'.'), (1,'#'), (3,'.'), (1,'#'), (7,'.'), (1,'#'), (3,'.'), (1,'#'), (4,'.')],
                     [(4,'.'), (1,'#'), (2,'.'), (1,'#'), (8,'.'), (1,'#'), (2,'.'), (1,'#'), (4,'.')],
                     [(5,'.'), (1,'#'), (1,'.'), (1,'#'), (9,'.'), (1,'#'), (1,'.'), (1,'#'), (4,'.')],
                     [(6,'.'), (2,'#'), (10,'.'), (2,'#'), (1,'.')],
                     [(7,'.'), (2,'#'), (11,'.'), (2,'#'), (2,'.')]
                     ]


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


p = [[(3, '.'), (4, '#'), (3, '.')],
        [(3, '.'), (1, '#'), (2, '.'), (1, '#'), (3, '.')],
        [(3, '.'), (4, '#'), (3, '.')],
        [(3, '.'), (1, '#'), (6, '.')],
        [(3, '.'), (1, '#'), (6, '.')]
        ]
l = [[(2, '.'), (1, '#'), (3, '.')],    -- last should be 7
        [(2, '.'), (1, '#'), (7, '.')],
        [(2, '.'), (1, '#'), (1, '.')], -- lsat should be 7
        [(2, '.'), (1, '#'), (4, '.')], -- last should be 7
        [(2, '.'), (6, '#'), (1, '.')] -- last should be 2
        ]





padPicture     :: PictureRLE -> PictureRLE
padPicture pic = [line ++ [(maxW - (sumN line), '.')] | line <- pic]
           where maxW = maximum (fmap sum [[n | (n,c) <- line] | line <- pic])
                 sumN aLine = sum [n | (n, c) <- aLine]

flipH :: PictureRLE -> PictureRLE
flipH = map reverse


-- note place one picture above another, join two lists of lines together
above :: PictureRLE -> PictureRLE -> PictureRLE
above = (++)


-- note flip picture in a vertical mirror
flipV :: PictureRLE -> PictureRLE
flipV = reverse


-- note place two pictures side by side, by joining up their corresponding lines.
beside :: PictureRLE -> PictureRLE -> PictureRLE
beside picL picR = [lineL ++ lineR | (lineL, lineR) <- zip picL picR]


-- note inverting the color of pictures
invertChar :: Char -> Char
invertChar ch = if ch == '.' then '#' else '.'

invertLine :: [(Integer, Char)] -> [(Integer, Char)]
invertLine line = [(n, invertChar ch) | (n, ch) <- line]

-- note now apply inversion to all lines in the picture
invertColour :: PictureRLE -> PictureRLE
invertColour pic = [invertLine line | line <- pic]



draw :: PictureRLE -> IO()
draw pic = putStr $ onSepLines pic
  where
    onSepLines pic = concat [concat [replicate (fromInteger n :: Int) c | (n, c) <- line] ++ "\n"| line <- pic ]


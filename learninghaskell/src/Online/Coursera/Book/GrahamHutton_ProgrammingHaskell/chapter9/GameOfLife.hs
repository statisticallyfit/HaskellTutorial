{-
Each internal square on the board has eight immediate neighbours.

For uniformity, each square on the edge of the board is also viewed as having
eight neighbours, by assuming that the board wraps around from top-to-bottom
and from left-to-right. That is, we can think of the board as really being a torus,
a three-dimensional doughnut shaped object.
Given an initial conﬁguration of the board, the next generation is given by
simultaneously applying the following rules to all squares:

* a living cell survives if it has precisely two or three neighbouring squares
that contain living cells, and dies (becomes empty) otherwise;
* an empty square gives birth to a living cell if it has precisely three neighbours
that contain living cells, and remains empty otherwise.

-}


-- moves cursor to a given position
goto :: Pos -> IO()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- displays string at given position
writeat :: Pos -> String -> IO()
writeat p xs = do goto p
                  putStr xs

cls :: IO ()
cls = putStr "\ESC[2J"

-- performs list of actions in sequence, discarding results returning no result.
seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as


----------------------------------------------------------------------------------------------

width :: Int
width = 5

height :: Int
height = 5

type Pos = (Int, Int)
type Board = [Pos]

row :: Int -> [Pos]
row r = [(x,y) | x <- [r] , y <- [1..5]]

board :: Board
board = row 1 ++ row 2 ++ row 3 ++ row 4 ++ row 5


glider :: Board -- glider is a type of cell arrangments (in book page 106)
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]


showCells :: Board -> IO()
showCells b = seqn [writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

-- returns neighbors of a position
neighbors :: Pos -> [Pos]
neighbors (x,y) = map wrap [(x-1, y-1), (x, y-1),
                            (x+1, y-1), (x-1, y),
                            (x+1, y), (x-1, y+1),
                            (x, y+1), (x+1, y+1)]

wrap :: Pos -> Pos
wrap (x,y) = ( ((x-1) `mod` width) + 1,
               ((y-1) `mod` height) + 1)


numLiveNeighbors :: Board -> Pos -> Int
numLiveNeighbors b = length . filter (isAlive b) . neighbors -- giving neighbors a position


-- RULE 1: considered living position if it has 2 or 3 live neighbors
survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (numLiveNeighbors b p) [2,3]]

-- RULE 2: births rule
-- note not considering all board positions. Just considering the neighbors of living cells
-- on the board because only those positions can potentially give new births.
births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbors b)), -- x <- [1..width], y <- [1..height],
                isEmpty b p,
                numLiveNeighbors b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs )


nextGen :: Board -> Board
nextGen b = survivors b ++ births b



life :: Board -> IO()
life b = do cls
            showCells b
            wait 5000
            life (nextGen b)

wait :: Int -> IO()
wait n = seqn [return () | _ <- [1..n]]
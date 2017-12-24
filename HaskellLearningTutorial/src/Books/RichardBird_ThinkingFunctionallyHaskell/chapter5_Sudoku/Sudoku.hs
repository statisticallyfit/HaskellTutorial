module Sudoku where


import Data.List (intercalate, intersperse)

{-
NOTE terminology

cell -> a single entry in the matrix
box -> a box consisting of 9 by 9 cells.
grid -> a grid which is 3 by 3 boxes.
row -> goes from start to end grid (horizontally) and is 9 cells long.
col -> goes from top to bottom of grid (vertically) and is 9 cells tall.

-}

-- note matrix is a list of rows. m x n matrix is a list of m rows, each length n.
type Matrix a = [Row a]
type Row a = [a]

type Grid = Matrix Digit
type Digit = Int

digits :: [Digit]
digits = [1 .. 9]

blank :: Digit -> Bool
blank = (== 0)



-- Game plan: start with the given grid and complete it by filling in every
-- possible choice for the blank entries. Then filter this list of filled grids
-- for those that don't contain duplicates in any row, box, or column.
-- note this is the game plan executor
-- note: using many prune because after one round of pruning some choices may be
-- resolved into singletons and another round may remove more impossible choices.
-- note using many to repeatedly prune the matrix of choices until only singleton
-- choices are left.
solve :: Grid -> Grid
solve = concat . filter valid . expand . many prune . choices
-- filter valid . completions
------------------------------------------------------------------------------------------
-- note remove any choices from a cell c that already occur as singleton entries in the
-- row, col, and box containing c.
prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxs . pruneBy cols . pruneBy rows


pruneBy f = f . map pruneRow . f



-- note remove the elements per digit list that already occur as singletons.
-- ["6", "12", "3", "134", "56"] ==> ["6","12","3","14","5"]
-- ["6", "36", "3", "134", "4"] ==>  ["6","","3","1","4"]
pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map (remove singletons) row
    where singletons = [d | [d] <- row] -- note gets just singletons



-- note removes ds from xs so in the end, there are no more ds in xs
remove :: [Digit] -> [Digit] -> [Digit]
remove ds [x] = [x]
remove ds xs = filter ({-x one by one-} `notElem` ds) xs


-- note: many prune finds a limit (if next answer is same as previous, return previous)
many :: Eq a => (a -> a) -> a -> a
many f x = if x == y then x else (many f y)
    where y = f x

---------------------------------------------


-- note there must be no duplicates in any box, row or column!
-- That's the mettle of the game.
valid :: Grid -> Bool
valid grid = all noDups (rows grid) &&
             all noDups (cols grid) &&
             all noDups (boxs grid)

noDups :: (Eq a) => [a] -> Bool
noDups [] = True
noDups (x:xs) = all (/= x) xs && noDups xs

-- note id because each matrix carries rows right there.
rows :: Matrix a -> Matrix a
rows = id

-- note returns the transpose
cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)


boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . group . map group

-- note splits a list into groups of three, like sudoku grid
group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs : group (drop 3 xs)


-- note takes a grouped list and ungroups it
ungroup :: [[a]] -> [a]
ungroup = concat


---------------------------------------------
{-completions :: Grid -> [Grid]
completions = expand . choices-}

-- postcondition: returns a Matrix where each cell is a list of digits.
choices :: Grid -> Matrix [Digit]
choices grid = map (map choice) grid
    -- note if a cell is blank then all digits are possible, else just the given digit.
    where choice d = if blank d then digits else [d]

-- note converts the matrix into list of grids by installing all choices
-- in all possible ways.
-- postcondition: first mixes up the elements in each row in each possible way, then mixes
-- up the rows in each possible way. Returns empty list if any element in any row
-- is the empty list.
expand :: Matrix [Digit] -> [Grid] -- [[[a]]] -> [[[a]]]
expand = cartesianProduct . map cartesianProduct

-- [[1,2,3],[2], [1,3]] ==> [[1,2,1],[1,2,3],[2,2,1],[2,2,3],[3,2,1],[3,2,3]]
-- [[2], [1,3]] => [[2,1], [2,3]]
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (xs : xss) = [x:ys | x <- xs, ys <- yss]
    where yss = cartesianProduct xss
-- note less efficient is above version: cprod is computed just once.
-- cartesianProduct (xs : xss) = [x:ys | x <- xs, ys <- cartesianProduct xss]






showGrid :: [[Int]] -> String
showGrid grid = seperatedLines
    where seperatedLines = break ++ (intercalate break lines) ++ break
          lines = map concat (group [showFullLine r ++ "\n" | r <- [0.. (gridWidth - 1)]])
          showFullLine r = "| " ++ intercalate " | " (map (showBoxLine r) [0..2]) ++ " |"
          showBoxLine r p = intersperse ' ' $ concatMap show ((group (grid !! r)) !! p)
          break = (replicate lineLenStr '-') ++ "\n"
          lineLenStr = length (showFullLine 0) -- length of arbitrary line
          gridWidth = length grid

printGrid :: [[Int]] -> IO()
printGrid = putStr . showGrid




e1 :: Row Digit
e1 = [2, 0, 0, 0, 0, 5, 3, 9, 0]
e2 = [8, 4, 0, 0, 0, 2, 0, 0, 0]
e3 = [0, 5, 0, 0, 3, 1, 8, 2, 0]
e4 = [0, 8, 0, 0, 4, 0, 2, 0, 3]
e5 = [0, 0, 2, 1, 0, 7, 5, 0, 0]
e6 = [6, 0, 9, 0, 2, 0, 0, 8, 0]
e7 = [0, 9, 1, 2, 5, 0, 0, 4, 0]
e8 = [0, 0, 0, 9, 0, 0, 0, 3, 5]
e9 = [0, 6, 4, 3, 0, 0, 0, 0, 2]

easyPuzzle :: Grid
easyPuzzle = [e1, e2, e3, e4, e5, e6, e7, e8, e9]


h1 :: Row Digit
h1 = [0, 0, 0, 4, 0, 0, 0, 0, 9]
h2 = [0, 4, 0, 7, 0, 0, 1, 0, 0]
h3 = [0, 0, 0, 0, 8, 0, 3, 2, 0]
h4 = [0, 0, 8, 2, 0, 0, 0, 0, 6]
h5 = [6, 0, 0, 5, 0, 8, 0, 0, 7]
h6 = [4, 0, 0, 0, 0, 6, 8, 0, 0]
h7 = [0, 2, 9, 0, 7, 0, 0, 0, 0]
h8 = [0, 0, 4, 0, 0, 1, 0, 5, 0]
h9 = [1, 0, 0, 0, 0, 9, 0, 0, 0]

veryHardPuzzle :: Grid
veryHardPuzzle = [h1, h2, h3, h4, h5, h6, h7, h8, h9]




main :: IO()
main = do
    printGrid $ solve easyPuzzle
    --putStrLn "\n"
    -- printGrid $ solve veryHardPuzzle -- help why does it take forever to solve?





--- testing empty list result when one list in args is empty:
-- cartesianProduct [[1,2], [], [4,5]] ===> []

--- testing basic combos
-- cartesianProduct [[1],[2],[3]] ==> [[1,2,3]]

--- testing more combo
-- cartesianProduct[[1,2,3],[2],[1,3]] ==> [[1,2,1],[1,2,3],[2,2,1],[2,2,3],[3,2,1],[3,2,3]]


--------- > laws

--- testing cols: test that mxn matrix results in nxm after running through cols()

--- testing rows . rows = id, cols . cols = id, boxs . boxs = id (3rd is valid on n^2 x n^2
-- matrices provided we change group to group by n.

--- testing ungroup . group = id, group . ungroup = id

--- testing map rows . expand = expand . rows (valid on n^2 x n^2 matrices of choices)
--- testing map cols . expand = expand . cols (valid on n^2 x n^2 matrices of choices)
--- testing map boxs . expand = expand . boxs (valid on n^2 x n^2 matrices of choices)

--- testing map (map f) . cp = cp . map (map f) -- note suggested by id type of (cp)
--- testing filter (all p) . cp = cp . map (filter p)

--- testing filter (p . f) = map f . filter p . map f
--- testing filter (p . f) . map f = map f . filter p


--- testing filter noDups . cp = filter nodups . cp . pruneRow
-- says that pruning a row will not throw away any list which contain no duplicates.


--- testing remove: when you add ds back into xs, length overall is len ds + len xs and
-- there are no duplicates in overall list.

-- note matrix is a list of rows. m x n matrix is a list of m rows, each length n.
type Matrix a = [Row a]
type Row a = [a]

type Grid = Matrix Digit
type Digit = Char

digits :: [Digit]
digits = ['1' .. '9']

blank :: Digit -> Bool
blank = (== '0')


-- Game plan: start with the given grid and complete it by filling in every
-- possible choice for the blank entries. Then filter this list of filled grids
-- for those that don't contain duplicates in any row, box, or column.
-- note this is the game plan executor
solve :: Grid -> [Grid]
solve = filter valid . completions
------------------------------------------------------------------------------------------
-- note no duplicates in rows, cols, and boxes.
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
completions :: Grid -> [Grid]
completions = expand . choices

-- note installs the available digits for each cell
-- postcondition: result is matrix where each cell is a list of digits.
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
-- note less efficient, above version: cprod is computed just once.
-- cartesianProduct (xs : xss) = [x:ys | x <- xs, ys <- cartesianProduct xss]




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

--- testing map rows . expand = expand . rows (valid on n^2 x n^2 matrices)
--- testing map cols . expand = expand . cols (valid on n^2 x n^2 matrices)
--- testing map boxs . expand = expand . boxs (valid on n^2 x n^2 matrices)

--- testing map (map f) . cp = cp . map (map f) -- note suggested by id type of (cp)
--- testing filter (all p) . cp = cp . map (filter p)

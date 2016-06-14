
type Picture = [[Char]]


flipH :: Picture -> Picture
flipH = reverse


flipV :: Picture -> Picture
flipV = map reverse

above :: Picture -> Picture -> Picture
above = (++)

beside :: Picture -> Picture -> Picture
beside = zipWith (++)

-- note map to each string the function (map invertChar) which means map to each
-- char and invert it.
invertColour :: Picture -> Picture
invertColour = map (map invertChar)
    where invertChar c = if c == '.' then '#' else '.'

-- note zip each string with function (zipWith combineChar) which means zip each
-- char in each string with combine function.
superimpose :: Picture -> Picture -> Picture
superimpose = zipWith (zipWith combineChar)
    where combineChar a b = if a == b && a == '.' then '.' else '#'



draw :: Picture -> IO()
draw = putStr . concat . map (++ "\n")
-- same as: putStr $ concat $ map ( ++ "\n") pic



p1 = ["##########", "######....", "###.......", "##........", "#........."]
p2 = ["##########", "###....###", "###....###", "###....###", "##########"]
p  = ["...####...", "...#..#...", "...####...", "...#......", "...#......"]
b  = ["...####...", "...#...#..", "...####...", "...#...#..", "...####..."]
l  = ["..#.......", "..#.......", "..#.......", "..#.......", "..######.."]
n  = ["..#....#..", "..#.#..#..", "..#..#.#..", "..#...##..", "..#....#.."]


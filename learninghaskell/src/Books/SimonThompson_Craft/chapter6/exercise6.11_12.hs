import Test.QuickCheck


type Picture = [[Char]]


-- note place one picture above another, join two lists of lines together
above :: Picture -> Picture -> Picture
above = (++)

beside :: Picture -> Picture -> Picture
beside picL picR = [lineL ++ lineR | (lineL, lineR) <- zip picL picR]


flipH :: Picture -> Picture
flipH = reverse

flipV :: Picture -> Picture
flipV pic = [reverse line | line <- pic]

draw :: Picture -> IO()
draw pic = putStr $ onSeperateLines pic
           where onSeperateLines pic = [letter | picLine <- pic, letter <- picLine ++ "\n"]


p1 = ["##########", "######....", "###.......", "##........", "#........."]
p2 = ["##########", "###....###", "###....###", "###....###", "##########"]
p3 = ["#.#", "..#"]
p = ["...####...", "...#..#...", "...####...", "...#......", "...#......"]
b = ["...####...", "...#...#..", "...####...", "...#...#..", "...####..."]
l = ["..#.......", "..#.......", "..#.......", "..#.......", "..######.."]
n = ["..#....#..", "..#.#..#..", "..#..#.#..", "..#...##..", "..#....#.."]




-- NOTE Testing

height pic = length [line | line <- pic]

prop_AboveFlipV :: Picture -> Picture -> Bool
prop_AboveFlipV pic1 pic2 = flipV (pic1 `above` pic2) == (flipV pic1) `above` (flipV pic2)

prop_AboveFlipH :: Picture -> Picture -> Bool
prop_AboveFlipH pic1 pic2 = flipH (pic1 `above` pic2) == (flipH pic2) `above` (flipH pic1)

prop_BesideFlipV :: Picture -> Picture -> Bool
prop_BesideFlipV pic1 pic2 = flipV (pic1 `beside` pic2) == (flipV pic2) `beside` (flipV pic1)

-- HElp how is this giving up?
prop_BesideFlipH :: Picture -> Picture -> Property
prop_BesideFlipH pic1 pic2 = (height pic1) == (height pic2)
    ==>
    flipH (pic1 `beside` pic2) == (flipH pic1) `beside` (flipH pic2)


main = do
    quickCheck prop_AboveFlipV
    quickCheck prop_AboveFlipH
    quickCheck prop_BesideFlipV
    quickCheck prop_BesideFlipH
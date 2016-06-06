import Test.QuickCheck

-- 6.4 PICTURE EXAMPLE --------------------------------------------------------------------

type Picture = [[Char]]

-- note flipping picture horizontally, just reverse order of lines in picture.
flipH :: Picture -> Picture
flipH = reverse


-- note place one picture above another, join two lists of lines together
above :: Picture -> Picture -> Picture
above = (++)


-- note flip picture in a vertical mirror
flipV :: Picture -> Picture
flipV pic = [reverse line | line <- pic]


-- note place two pictures side by side, by joining up their corresponding lines.
beside :: Picture -> Picture -> Picture
beside picL picR = [lineL ++ lineR | (lineL, lineR) <- zip picL picR]


-- note inverting the color of pictures
invertChar :: Char -> Char
invertChar ch = if ch == ' ' then '#' else ' '

invertLine :: [Char] -> [Char]
invertLine line = [invertChar ch | ch <- line]

-- note now apply inversion to all lines in the picture
invertColour :: Picture -> Picture
invertColour pic = [invertLine line | line <- pic]




draw :: Picture -> IO()
draw pic = putStr $ onSeperateLines pic
 where
    onSeperateLines pic = [letter | picLine <- pic, letter <- picLine ++ "\n"]


p1 = ["##########", "######....", "###.......", "##........", "#........."]
p2 = ["##########", "###....###", "###....###", "###....###", "##########"]
p = ["...####...", "...#..#...", "...####...", "...#......", "...#......"]
b = ["...####...", "...#...#..", "...####...", "...#...#..", "...####..."]
l = ["..#.......", "..#.......", "..#.......", "..#.......", "..######.."]
n = ["..#....#..", "..#.#..#..", "..#..#.#..", "..#...##..", "..#....#.."]



{-
p1 = ["##########", "######    ", "###       ", "##        ", "#         "]
p2 = ["##########", "###    ###", "###    ###", "###    ###", "##########"]
p3 = ["   ####   ", "   #  #   ", "   ####   ", "   #      ", "   #      "]
-}




-- NOTE Testing

prop_AboveFlipV :: Picture -> Picture -> Bool
prop_AboveFlipV p1 p2 = flipV (p1 `above` p2) == (flipV p1) `above` (flipV p2)

-- help fails because quickcheck generates random data with lists not of same length ...
prop_AboveFlipH :: Picture -> Picture -> Bool
prop_AboveFlipH p1 p2 = flipH (p1 `above` p2) == (flipH p1) `above` (flipH p2)



-- note ==> means implies and property following ==> is only checked when condition before
-- it is true.
-- HELP doesn't work, what does this test mean ?
{-

propAboveBeside3Correct :: Picture -> Picture -> Property
propAboveBeside3Correct w e = (rectangular w $$ rectangular e $$ height w == height e)
    ==>
        (w `beside` e) `above` (w `beside` e)
            ==
        (w `above` w) `beside` (e `above` e)
-}

{-
uncover
main = do
    quickCheck prop_AboveFlipV
    --quickCheck prop_AboveFlipH
    -- quickCheck propAboveBeside3Correct-- HELP
-}












-- 6.7 SUPERMARKET BILLING ------------------------------------------------------------

type Name    = String
type Price   = Int
type BarCode = Int

type Database = [(BarCode, Name, Price)]
type TillType = [BarCode]
type BillType = [(Name, Price)]

{-
NOTE supermarket bill example

dry sherry, 1lt ...............5.40
fish fingers ..................1.21
orange jelly ..................0.56
dry sherry, 1lt ...............5.40

total ........................13.90
-}


codeIndex :: Database
codeIndex = [ (4719, "Fish Fingers", 121),
              (5643, "Nappies", 1010),
              (3814, "Orange Jelly", 56),
              (1111, "Hula Hoops", 21),
              (1112, "Hula Hoops (Giant)", 133),
              (1234, "Dry Sherry, 1lt", 540)]

-- NOTE: GOAL: convert list of bar codes into a list of name,price pairs

makeBill :: TillType -> BillType

formatBill :: BillType -> String

produceBill :: TillType -> String
produceBill = formatBill . makeBill


lineLength :: Int
lineLenght = 30 -- bill line length


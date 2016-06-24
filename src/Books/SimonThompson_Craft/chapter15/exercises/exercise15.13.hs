
-- leaf carries letter and its frequency
data Tree = Leaf Char Int | Node Int Tree Tree deriving (Eq, Show)
data Bit = L | R deriving (Eq, Show)
type HuffmanCode = [Bit]
type Table = [(Char, HuffmanCode)] -- when huffman tree is converted to table


-- Coding.hs --------------------------------------------------------------------------------

-- note look up each char in the tbale and concatenate the results
codeMessage :: Table -> String -> HuffmanCode
codeMessage table = concat . map (lookupTable table) -- the arg of char list here

-- loooking up the value n corresponding to  a ke.y char.
lookupTable :: Table -> Char -> HuffmanCode
lookupTable [] c = error "lookupTable"
lookupTable ((ch,n):tb) c
    | ch == c = n
    | otherwise = lookupTable tb c

-- note to decode a sequence of bits we use a tree
decodeMessage :: Tree -> HuffmanCode -> String
decodeMessage tree = decodeByTree tree
        where
            decodeByTree (Leaf c n) rest = c : decodeByTree tree rest
            decodeByTree (Node n t1 t2) (L:rest) = decodeByTree t1 rest
            decodeByTree (Node n t1 t2) (R:rest) = decodeByTree t2 rest
            decodeByTree t [] = []

-- Frequency.hs -------------------------------------------------------------------------------

frequency :: [Char] -> [(Char, Int)]
frequency = mergeSort freqMerge . mergeSort alphaMerge . map start -- string arg here
    where start ch = (ch, 1)

-------------------------------------------------
mergeSort :: ([a] -> [a] -> [a]) -> [a] -> [a]
mergeSort merge xs
    | length xs < 2 = xs
    | otherwise = merge (mergeSort merge first) (mergeSort merge second)
    where first = take half xs
          second = drop half xs
          half = (length xs) `div` 2

-- note sorting on chars
-- input: all ints are equal to 1
-- output: all tuples are in sorted char order and ints are amalgamated for identical chars.
alphaMerge :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
alphaMerge xs [] = xs
alphaMerge [] ys = ys
alphaMerge ((p,n):xs) ((q,m):ys)
    | p == q    = (p, n+m) : alphaMerge xs ys
    | p < q     = (p, n) : alphaMerge xs ((q,m):ys)
    | otherwise = (q, m) : alphaMerge ((p,n):xs) ys

-- note sorting on freqs
freqMerge :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
freqMerge xs [] = xs
freqMerge [] ys = ys
freqMerge ((p,n):xs) ((q,m):ys)
    | n < m || (n == m && p < q) = (p, n) : freqMerge xs ((q,m):ys)
    | otherwise = (q,m) : freqMerge ((p,n):xs) ys


-- MakeTree.hs --------------------------------------------------------------------------------

makeTree :: [(Char, Int)] -> Tree
makeTree = makeCodes . toTreeList

-- note converts each char-num pair into a tree
{-
(uncurry Leaf) :: (Char, Int) -> Tree
Leaf :: Char -> Int -> Tree
so now (uncurry Leaf) is a function that takes a type (Char,Int) and produces a Tree.
Now we map that single function over the list of (Char,Int) tuples.
-}
toTreeList :: [(Char,Int)] -> [Tree]
toTreeList = map (uncurry Leaf)

-- note amalgamates trees successiviely into a single tree.
makeCodes :: [Tree] -> Tree
makeCodes [t] = t
makeCodes ts = makeCodes (amalgamate ts)

-- note pair first two trees and plug them back in tree list in sorted order.
amalgamate :: [Tree] -> [Tree]
amalgamate (t1:t2:ts) = insTree (pair t1 t2) ts

insTree :: Tree -> [Tree] -> [Tree]
insTree tree [] = [tree]
insTree tree (t:ts)
    | value tree <= value t = tree : t : ts
    | otherwise = t : insTree tree ts

-- note to pair, combine frequency counts
pair :: Tree -> Tree -> Tree
pair t1 t2 = Node (v1 + v2) t1 t2
             where v1 = value t1
                   v2 = value t2

value :: Tree -> Int
value (Leaf _ n) = n
value (Node n _ _) = n


-- MakeCode.hs ----------------------------------------------------------------------------------

-- combining frequency calculation and tree converstion
codes :: [Char] -> Tree
codes = makeTree . frequency


-- CodeTable.hs --------------------------------------------------------------------------------

-- note converts a huffman tree into table
codeTable :: Tree -> Table
codeTable = convert []  --- tree arg

convert :: HuffmanCode -> Tree -> Table
convert hcode (Leaf c n) = [(c, hcode)]
convert hcode (Node n t1 t2) = (convert (hcode ++ [L]) t1)
                            ++ (convert (hcode ++ [R]) t2)







-- exercise 13 --------------------------------------------------------------------------------

-- data Tree = Leaf Char Int | Node Int Tree Tree

depth :: Tree -> Int
depth (Leaf _ _) = 1
depth (Node _ t1 t2) = 1 + maximum [depth t1, depth t2]

-- note cumulative count of num spaces starting from bottom middle of node (mutually
 -- exclusive of rightWidth) and ending to leftmost leaf. Including pad separating
 -- tree and left screen edge.
indent :: Tree -> Int
indent (Leaf _ _) = 2 -- means add 2 spaces left beyond left arm
indent (Node _ t1 _) = 2 + indent t1 -- means 1 space under node,1 for left arm.


-- NOTE has 2 rows of nodes ==> 3 spaces between all leaves
row1'    = "      ____(2)____      "
row2'    = "     |           |     "
row3'    = "   _(4)_       _(5)_  "
row4'    = "  |     |     |     | "
row5'    = " n:6   m:5   m:4   j:8"
test2 = row1' ++ "\n" ++ row2' ++ "\n" ++ row3' ++ "\n" ++
        row4' ++ "\n" ++ row5' ++ "\n"


row1'' = "   _(2)_  "
row2'' = "  |     | "
row3'' = " n:4   m:8"
test3 = row1'' ++ "\n" ++ row2'' ++ "\n" ++ row3'' ++ "\n"


r1'    = "         _________2________           "
r2'    = "        |                  |          "
r3'    = "    ____4____          ____5____     "
r4'    = "   |         |        |         |    "
r5'    = "  _6_       _9_      _3_       _1_  "
r6'    = " |   |     |   |    |   |     |   | "
r7'    = "n:4 m:8   m:2 g:9  h:2 k:5   b:3 y:2"
test4 = r1' ++ "\n" ++ r2' ++ "\n" ++ r3' ++ "\n" ++
        r4' ++ "\n" ++ r5' ++ "\n" ++ r6' ++ "\n" ++ r7' ++ "\n"


-- NOTE has 3 rows of ndoes ==> 3 spaces between all leaves
r1    = "           __________(2)__________           "
r2    = "          |                       |          "
r3    = "     ____(4)____             ____(5)____     "
r4    = "    |           |           |           |    "
r5    = "  _(6)_       _(9)_       _(3)_       _(1)_  "
r6    = " |     |     |     |     |     |     |     | "
r7    = "n:4   m:8   m:2   g:9   h:2   k:5   b:3   y:2"

test5 = r1 ++ "\n" ++ r2 ++ "\n" ++ r3 ++ "\n" ++
        r4 ++ "\n" ++ r5 ++ "\n" ++ r6 ++ "\n" ++ r7 ++ "\n"



-- NOTE has 4 rows of nodes ==> 3 spaces between all leaves
                               -- 22 dashes                  -- 21 dashes
rr1 = "                       ______________________(12)_____________________                        "
rr2 = "                      |                                               |                       "
rr3 = "           __________(4)__________                         __________(1)__________            "
rr4 = "          |                       |                       |                       |           "
rr5 = "     ____(6)____             ____(9)____             ____(1)____             ____(1)____      "
rr6 = "    |           |           |           |           |           |           |           |     "
rr7 = "  _(9)_       _(8)_       _(2)_       _(2)_       _(1)_       _(1)_       _(1)_       _(1)_   "
rr8 = " |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |  "
rr9 = "n:9   m:3   h:1   i:1   j:1   m:1   k:9   o:1   n:9   m:3   h:1   i:1   j:1   m:1   k:9   o:1 "

test6 = rr1 ++ "\n" ++ rr2 ++ "\n" ++ rr3 ++ "\n" ++ rr4 ++ "\n" ++ rr5
        ++ "\n" ++ rr6 ++ "\n" ++ rr7 ++ "\n" ++ rr8 ++ "\n" ++ rr9 ++ "\n"








t1 = Node 12 (Leaf 'a' 4) (Leaf 'm' 3)
t2 = Node 12 (Leaf 'a' 4) (Node 8 (Leaf 't' 1) (Leaf 'm' 5))
t3 = Node 12 (Leaf 'a' 4) (Node 8 (Node 4 (Leaf 'g' 2) (Leaf 'm' 2)  )
                                  (Node 4 (Node 2 (Leaf 'n' 1) (Leaf 't' 1))
                                          (Leaf 'k' 10) ))




-- NOTE if the node has more than 1 digit, take 1 dash from right, then another dash from left
-- until the amount is correct.
-- NOTE if leaf (n) has more than 1 digit, take 1 space from right then one from left until
-- amount is correct.
{-

showTree :: Tree -> String
showTree (Leaf c n) = [c] ++ ":" ++ show n
showTree (Node n t1 t2) =
    where top = "    " ++ show n ++ "\n"
-}







-- note 9 lines under letter + 1 for space before Code title
showTable :: Table -> String
showTable table = "\n" ++ title ++ "\n" ++ linesUnderTitle ++ "\n" ++ content ++ "\n"
    where maxCodeLen = maximum $ map (\(l,cs) -> length cs) table
          showTuple (l, cs) = " " ++ show l ++ "    | " ++ concatMap (\bit -> show bit) cs ++ "\n"
          title = " Letter | Code"
          linesUnderTitle = replicate (10 + maxCodeLen) '-'
          content = concatMap showTuple table

printTable :: Table -> IO()
printTable table = putStr $ showTable table



text1 = "amalgamating"
freqs1 = frequency text1
treeList1 = toTreeList freqs1
tree1 = makeCodes treeList1
table1 = codeTable tree1

text2 = "banananation"
freqs2 = frequency text2
treeList2 = toTreeList freqs2
tree2 = makeCodes treeList2
table2 = codeTable tree2


text3 = "anabananaananas"
freqs3 = frequency text3
treeList3 = toTreeList freqs3
tree3 = makeCodes treeList3
table3 = codeTable tree3










--------------------------------------------------------------------------------------

{-

--NOTE trying another view (tree is flipped)
vvv2 = [" |-- n:3", "(2)", " |-- m:5"]

vvv1 = [" |", " |", " |", " |", " |", " |", " |", " |", " |", "(2)",
        " |", " |", " |", " |", " |", " |", " |", " |", " |"]


type Picture = [[Char]]

flipV :: Picture -> Picture
flipV pic = [reverse line | line <- pic]


-- note: rotate clockwise 90 degrees
rotate :: Picture -> Picture
rotate pic = flipV [ [xs !! i | xs <- pic] | i <- [0 .. rowLength - 1]]
             where rowLength = length (pic !! 0)

-- note rotate counterclockwise 90 degrees
rotateCounter :: Picture -> Picture
rotateCounter pic = rotate $ rotate $ rotate pic

draw :: Picture -> IO()
draw pic = putStr $ concatMap (++"\n") pic

-}
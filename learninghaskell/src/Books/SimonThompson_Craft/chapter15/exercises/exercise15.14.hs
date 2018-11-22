import Test.QuickCheck hiding (frequency)


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
--amalgamate [t] = [t] -- note my add, not needed since in makeCodes [t] = t
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







text1 = "banananationwinner"
tree1 = codes text1
table1 = codeTable tree1
hcode1 = codeMessage table1 text1


-- HELP TODO - why is amalgamate failing?
-- first coding then decoding should result in same string
propCodeDecodeID :: String -> Bool
propCodeDecodeID string = (decodeMessage tree (codeMessage table string)) == string
    where tree = codes string
          table = codeTable tree
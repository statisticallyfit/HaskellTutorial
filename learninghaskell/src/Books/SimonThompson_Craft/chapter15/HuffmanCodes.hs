
-- Combining all the files Types.hs, Coding.hs...

-- NOTE Huffman code is built so that most frequent letters have the shortest sequences
-- of code bits and the less frequent have more expensive code bits.
 -- Therefore the best tree is one where the most frequent letters in your code would
 -- be at the top nodes so they have the shortest sequence in code.


{-
NOTE how to find the tree with optimal code for the text:

1. find frequencies of individual letters : tuple resulting is (letter, freq)

2. each tuple is turned into a tree. So ('b', 1) ==> Leaf 'b' 1
These are sorted in frequency order

3. amalgamate together trees by combining the two trees of lowest frequency. Insert
into proper sorted order. Repeat until one tree is left.

-}


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




text1 = "amalgamating"
freqs1 = frequency text1
treeList1 = toTreeList freqs1
tree1 = makeCodes treeList1
table1 = codeTable tree1

text2 = "banananation"
freqs2 = frequency text2
treeList2 = toTreeList freqs2
tree2 = makeCodes treeList2


text3 = "anabananaananas"
freqs3 = frequency text3
treeList3 = toTreeList freqs3
tree3 = makeCodes treeList3
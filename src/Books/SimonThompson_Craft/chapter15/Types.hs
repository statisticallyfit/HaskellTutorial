module Types (Tree (Leaf, Node), Bit (L, R), HuffmanCode, Table) where


-- leaf carries letter and its frequency
data Tree = Leaf Char Int | Node Int Tree Tree deriving (Eq, Show)

data Bit = L | R deriving (Eq, Show)

type HuffmanCode = [Bit]

type Table = [(Char, HuffmanCode)] -- when huffman tree is converted to table



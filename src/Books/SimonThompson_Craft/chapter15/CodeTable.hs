-- module CodeTable (codeTable) -- Tree -> Table
import Test.QuickCheck
import Types


-- note converts a huffman tree into table
codeTable :: Tree -> Table
codeTable = convert []          --- tree arg
--                  |                  |
--              huffcode list         tree

convert :: HuffmanCode -> Tree -> Table
convert hcode (Leaf c n) = [(c, hcode)]
convert hcode (Node n t1 t2) = (convert (hcode ++ [L]) t1)
                            ++ (convert (hcode ++ [R]) t2)



{-
NOTE

codeTable (Node 6 (Node 3 (Leaf 'b' 1) (Leaf 'a' 2)) (Leaf 't' 3))
= convert [] (Node 6 (Node 3 (Leaf 'b' 1) (Leaf 'a' 2)) (Leaf 't' 3))
= convert [L] (Node 3 (Leaf 'b' 1) (Leaf 'a' 2)) ++
  convert [R] (Leaf 't' 3)
= convert [L,L] (Leaf 'b' 1) ++ convert [L,R] (Leaf 'a' 2) ++
  [('t', [R])]
= [('b', [L,L]), ('a', [L,R]), ('t', [R])]

-}
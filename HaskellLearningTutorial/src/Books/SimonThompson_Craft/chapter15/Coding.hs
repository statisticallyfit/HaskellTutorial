--module Coding (codeMessage, decodeMessage)

import Types (Tree (Leaf, Node), Bit (L,R), HuffmanCode, Table)




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




tree1 = Node 0 (Leaf 'a' 0) (Node 0 (Leaf 'b' 0) (Leaf 't' 0))
huff1 = [R,L, L, R, R, R, R, L, R, R]


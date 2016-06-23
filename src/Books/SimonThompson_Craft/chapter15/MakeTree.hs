-- module MakeTree (makeTree) -- [(Char, Int)] -> Tree

import Types





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
insTree tree allTrees@(Leaf c leafNum : ts)
    | nodeNum <= leafNum = tree : allTrees
    | otherwise = insTree tree ts
    where (Node nodeNum t1 t2) = tree

-- note to pair, combine frequency counts
pair :: Tree -> Tree -> Tree
pair t1 t2 = Node (v1 + v2) t1 t2
             where v1 = value t1
                   v2 = value t2

value :: Tree -> Int
value (Leaf _ n) = n
value (Node n _ _ ) = n
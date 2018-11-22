


data Tree = Leaf Int | Node Tree Tree deriving (Eq, Show)

t1 :: Tree
t1 = Node (Node (Leaf 1) (Leaf 4))    ( Node (Leaf 6)  (Node (Leaf 9) (Node (Leaf 6) (Leaf 7))) )


t2 :: Tree
t2 = Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 6) (Leaf 9))




halve    :: [Int] -> ([Int], [Int])
halve xs = splitAt ((length xs) `div` 2) xs


balance :: [Int] -> Tree
balance [x] = Leaf x
balance xs = Node (balance ls) (balance rs)
             where (ls, rs) = halve xs




main = do
    print $ balance [1..8]
    putStrLn ""
    print $ balance [1..9]
    putStrLn ""
    print $ balance [1,2]
    print $ balance [1,2,3]
{-
NOTE here are the default implementations

-- help understand how this works

repeat   :: a -> [a]
repeat x = xs where xs = x : xs

take              :: Int -> [a] -> [a]
take 0 _          = []
take (n+1) []     = []
take (n+1) (x:xs) = x : take n xs

replicate   :: Int -> a -> [a]
replicate n = take n . repeat
-}


data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show)

n2 = Node (Node (Leaf) 1 (Leaf)) 2 (Node (Leaf) 3 (Leaf))
n6 = Node (Node (Leaf) 5 (Leaf)) 6 (Node (Leaf) 7 (Leaf))
n10 = Node (Node (Leaf) 9 (Leaf)) 10 (Node (Leaf) 11 (Leaf))
n14 = Node (Node (Leaf) 13 (Leaf)) 14 (Node (Leaf) 15 (Leaf))


t = Node (Node (n2) 4 (n6)) 8 (Node (n10) 12 (n14))


repeatTree   :: a -> Tree a
repeatTree x = Node t x t where t = repeatTree x


takeTree                :: Int -> Tree a -> Tree a
takeTree 0 _            = Leaf
takeTree n Leaf         = Leaf
takeTree n (Node l m r) = Node (takeTree (n-1) l) m (takeTree (n-1) r)


replicateTree   :: Int -> a -> Tree a
replicateTree n = takeTree n . repeatTree
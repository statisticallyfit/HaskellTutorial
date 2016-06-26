

data UniqueQueue a = UniqueQueue [a] deriving (Eq, Show)

-- note adds to end. Does not add if duplicated somewhere in the list. 
add :: a -> UniqueQueue a -> UniqueQueue a
add x q@(UniqueQueue xs)
    | x /= last xs = UniqueQueue (xs ++ [x])
    | otherwise = UniqueQueue xs -- do not add if duplicate with last.

-- note remove from front
remove :: UniqueQueue a -> UniqueQueue a
remove (UniqueQueue xs) = UniqueQueue (tail xs)
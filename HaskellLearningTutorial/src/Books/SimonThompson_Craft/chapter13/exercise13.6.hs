


data Shape = Circle Float | Rectangle Float Float deriving (Eq, Ord, Show)


area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle l w) = l * w




class Info a where
    examples :: [a]
    size     :: a -> Int
    size _   = 1


instance Info Float where
    examples = [3.4, 5.8, 71.3]



-- HELP how to write this if it were (Shape a)?
instance Info Shape  where
    examples = [Circle ((examples :: [Float]) !! 0),
                Rectangle ((examples :: [Float]) !! 1) ((examples :: [Float]) !! 2)]
    -- note HELP is this overriding the top definition?
    size     = round . area


[n1, n2, n3] = examples :: [Float]
[s1, s2] = examples :: [Shape]








main = do
    print $ [n1, n2, n3] -- HELP why can't print ordinary way, directly?
    print $ [s1, s2]
    print $ (size :: Float -> Int) (13.2)
    print $ (size :: Shape -> Int) (Circle 13.4)
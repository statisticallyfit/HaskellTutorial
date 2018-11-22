
data Shape = Circle Float | Rectangle Float Float deriving (Eq, Ord, Show)


perimeter :: Shape -> Float
perimeter (Circle r) = 2*pi*r
perimeter (Rectangle l w) = 2*l + 2*w


main = do
    print $ perimeter (Circle 3)
    print $ perimeter (Rectangle 4 3)



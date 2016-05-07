import Prelude


-- todo: why couldn't this use Space a? If I had that, then
-- todo: there would be error when I put (Space a) in instance method.
data RootQuantity = Finite Int | Infinite
    deriving (Show, Eq)
    --deriving Show

-- note: in case you want to show differently
{-}instance Show RootQuantity where
    show Infinite       =  "Infinite"
    show (Finite anInt) = show anInt
-}


-- numberOfNonDegenerateRoots
-- assume coefficient 'a' is not zero (the non-degenerate case)
numberNDRoots :: Float -> Float -> Float -> RootQuantity
numberNDRoots a b c
    | b^2 >  4.0 * a * c = Finite 2
    | b^2 == 4.0 * a * c = Finite 1
    | b^2 <  4.0 * a * c = Finite 0


-- assume the coefficient a is 0 (the degenerate case
numberDRoots :: Float -> Float -> RootQuantity
numberDRoots b c
    | b /= 0.0  = Finite 1
    | c /= 0.0  = Finite 0
    | otherwise = Infinite


numberRoots :: Float -> Float -> Float -> RootQuantity
numberRoots a b c
    | a /= 0.0  = numberNDRoots a b c
    | otherwise = numberDRoots b c



-- Exercise 3.17 -----------------------------------------------------
data Roots = Two (Float, Float) | One Float | None | Infinity
    deriving (Show, Ord, Eq)


calculateRoots :: Float -> Float -> Float -> Roots
calculateRoots a b c
    | numberRoots a b c == Finite 0                     = None
    | numberRoots a b c == Finite 1 && a == 0 && c == 0 = One 0
    | numberRoots a b c == Finite 1 && a == 0           = One ((-c)/b)
    | numberRoots a b c == Finite 1 && a /= 0           = One firstRoot
    | numberRoots a b c == Finite 2                     = Two (firstRoot, secondRoot)
    | numberRoots a b c == Infinite                     = Infinity
    where firstRoot  = ((-b) - sqrt (b^2 - 4.0*a*c)) / (2*a)
          secondRoot = ((-b) + sqrt (b^2 - 4.0*a*c)) / (2*a)




data Possibly = Null | Exist Float

instance Show Possibly where
    show (Exist x) = show x
    show Null  = "Nothing"


smallerRoot :: Float -> Float -> Float -> Possibly
smallerRoot a b c = let possibleRoots = calculateRoots a b c in
                    case possibleRoots of
                        One x      -> Exist x
                        Two (x, y) -> if x < y then Exist x else Exist y
                        _          -> Null

largerRoot :: Float -> Float -> Float -> Possibly
largerRoot a b c = let possibleRoots = calculateRoots a b c in
                    case possibleRoots of
                        One x      -> Exist x
                        Two (x, y) -> if x >= y then Exist x else Exist y
                        _          -> Null



funny x = x+x
peculiar y = y

main = do
    print (numberNDRoots 2 1 1)     -- 0
    print (numberNDRoots 1 6 9 )    -- 1
    print (numberNDRoots 1 2 (-15)) -- 2
    putStrLn ""
    ------------------------------------
    print (numberDRoots 1 0) -- 1
    print (numberDRoots 1 1) -- 1
    print (numberDRoots 0 1) -- 0
    print (numberDRoots 0 0) --infinity
    putStrLn ""
    ------------------------------------
    print (numberRoots 1 2 (-15)) -- 2
    print (numberRoots 4 4 1) -- 1
    print (numberRoots 0 1 1) -- 1
    print (numberRoots 0 1 0) -- 1
    print (numberRoots 5 2 1) -- 0
    print (numberRoots 0 0 1) -- 0
    print (numberRoots 0 0 0) --infinity
    putStrLn ""
    -------------------------------------
    print (calculateRoots 1 2 (-15)) -- 2
    print (calculateRoots 4 4 1) -- 1
    print (calculateRoots 0 1 1) -- 1
    print (calculateRoots 0 1 (-1)) -- 1
    print (calculateRoots 0 1 0) -- 1  --- get this to show One 0.0 not One (-0.0)
    print (calculateRoots 0 (-1) 0) -- 1
    print (calculateRoots 5 2 1) -- 0
    print (calculateRoots 0 0 1) -- 0
    print (calculateRoots 0 0 0) -- infinity
    putStrLn ""
    -------------------------------------
    print (smallerRoot 1 2 (-15), largerRoot 1 2 (-15)) -- 3, -5
    print (smallerRoot 1 1 (-4), largerRoot 1 1 (-4))
    print (smallerRoot 1 (-2) (-3), largerRoot 1 (-2) (-3))
    print (smallerRoot 4 4 1, largerRoot 4 4 1)
    print (smallerRoot 0 1 1, largerRoot 0 1 1)
    print (smallerRoot 0 1 (-1), largerRoot 0 1 (-1))
    print (smallerRoot 0 1 0, largerRoot 0 1 0)
    print (smallerRoot 0 (-1) 0, largerRoot 0 (-1) 0)
    print (smallerRoot 5 2 1, largerRoot 5 2 1)
    print (smallerRoot 0 0 1, largerRoot 0 0 1)
    print (smallerRoot 0 0 0, largerRoot 0 0 0)

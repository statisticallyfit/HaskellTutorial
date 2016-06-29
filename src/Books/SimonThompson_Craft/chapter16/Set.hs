import Data.List hiding (union, intersect)

-- note set is ordered list of nonduplicate elements
data Set a = Set [a] deriving Show


instance Eq a => Eq (Set a) where
    (==) = eqSet

instance Ord a => Ord (Set a) where
    (<=) = leqSet



empty :: Set a
empty = Set []

sing :: a -> Set a
sing x = Set [x]

-- note is member of this set?
memSet :: Ord a => Set a -> a -> Bool
memSet (Set []) y = False
memSet (Set (x:xs)) y
    | x < y = memSet (Set xs) y
    | x == y = True
    | otherwise = False

union :: Ord a => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (uni xs ys)
    where uni [] ys = ys
          uni xs [] = xs
          uni (x:xs) (y:ys)
              | x < y = x : uni xs (y:ys)
              | x == y = x : uni xs ys
              | otherwise = y : uni (x:xs) ys

intersect :: Ord a => Set a -> Set a -> Set a
intersect (Set xs) (Set ys) = Set (int xs ys)
    where int [] ys = []
          int xs [] = []
          int (x:xs) (y:ys)
              | x < y = int xs (y:ys)
              | x == y = x : int xs ys
              | otherwise = int (x:xs) ys

subSet :: Ord a => Set a -> Set a -> Bool
subSet (Set xs) (Set ys) = subS xs ys
    where subS [] ys = True
          subS xs [] = False
          subS (x:xs) (y:ys)
              | x < y = False
              | x == y = subS xs ys
              | otherwise = subS (x:xs) ys

eqSet :: Eq a => Set a -> Set a -> Bool
eqSet (Set xs) (Set ys) = (xs == ys)

-- note less than or equal?
leqSet :: Ord a => Set a -> Set a -> Bool
leqSet (Set xs) (Set ys) = (xs <= ys)

-- note precondition of remDups is sort. that is why we don't need to check x > y
makeSet :: Ord a => [a] -> Set a
makeSet = Set . remDups . sort
    where
    remDups [] = []
    remDups [x] = [x]
    remDups (x:y:xs)
        | x < y = x : remDups (y:xs)
        | otherwise = remDups (y:xs)

mapSet :: Ord b => (a -> b) -> Set a -> Set b
mapSet f (Set xs) = makeSet (map f xs) -- note definition of Functor

filterSet :: (a -> Bool) -> Set a -> Set a
filterSet p (Set xs) = Set (filter p xs)

foldSet :: (a -> a -> a) -> a -> Set a -> a
foldSet f s (Set xs) = foldr f s xs -- note s == seed


{-
note meaning of foldr

foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr f s []     = s
foldr f s (x:xs) = f x (foldr f s xs)

EXAMPLES
concat :: [[a]] -> [a]
concat xss = foldr (++) [] xs
and :: [Bool] -> Bool
and bs = foldr (&&) True bs

-}

-- example: showSet show (Set [1..10]) = 1 ... all on each line.
showSet :: (a -> String) -> Set a -> String
showSet f (Set xs) = concat (map ((++ "\n") . f) xs)

cardinality :: Set a -> Int
cardinality (Set xs) = length xs
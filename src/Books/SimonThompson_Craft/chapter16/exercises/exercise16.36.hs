import Data.List hiding (union, intersect)
import Test.QuickCheck


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

-- example: showSet show (Set [1..10]) = 1 ... all on each line.
showSet :: (a -> String) -> Set a -> String
showSet f (Set xs) = concat (map ((++ "\n") . f) xs)

cardinality :: Set a -> Int
cardinality (Set xs) = length xs












--- exercise 36 ------------------------------------------------------------------------

-- note diff is made by unionizing then removing elements that are in intersection,
-- leaving only the difference.
diff :: Ord a => Set a -> Set a -> Set a
diff s1 s2 = Set (eliminateFrom commons alls)
    where Set alls = union s1 s2
          Set commons = intersect s1 s2

-- note eliminates all xs from list of ys.
-- precondition: length xs <= length ys
eliminateFrom :: Ord a => [a] -> [a] -> [a]
eliminateFrom [] [] = []
eliminateFrom [] ys = ys
eliminateFrom (x:xs) (y:ys)
    | x == y = eliminateFrom xs ys
    | x < y = x : eliminateFrom xs (y:ys)
    | otherwise = y : eliminateFrom (x:xs) ys










--- TESTING ------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Set a) where
    arbitrary = do
        xs <- arbitrary
        return (Set xs)

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs)
    | x <= y = sorted (y:xs)
    | otherwise = False

numOccurs :: Eq a => a -> [a]-> Int
numOccurs _ [] = 0
numOccurs elm xs = length $ elemIndices elm xs

-- note returns list of num occurs of list ys in list xs
-- note ys can be longer than xs since we can return 0 if element in ys doesn't exist
-- in xs.
-- precondition: no list has to be in order.
-- postcondition: length numoccurs == length ys
numOccursAll :: Eq a => [a] -> [a] -> [Int]
numOccursAll xs [] = []
numOccursAll xs (elm:elems) = [numOccurs elm xs] ++ numOccursAll xs elems


-- tests if each element occurs only once.
unique :: Ord a => [a] -> Bool
unique xs = sum (numOccursAll xs xs) == (length xs)



-- testing unique . sorted . makeSet
testMake :: Ord a => [a] -> Bool
testMake xs = sorted xs' && unique xs'
    where Set xs' = makeSet xs

-- testing memSet == elem
testMember :: Int -> [Int] -> Bool
testMember n xs  = elem n xs' == memSet (Set xs') n
    where (Set xs') = makeSet xs

-- testing intersect xs ys == intersect ys xs
testIntersectID :: Ord a => Set a -> Set a -> Bool
testIntersectID s1 s2 = intersect s1 s2 == intersect s2 s1

-- testing union xs ys == union ys xs
testUnionID :: Ord a => Set a -> Set a -> Bool
testUnionID s1 s2 = union s1 s2 == union s2 s1

-- testing diff s1 s2 == diff s2 s1
testDiffID :: Ord a => Set a -> Set a -> Bool
testDiffID s1 s2 = diff s1 s2 == diff s2 s1

-- testing diff s1 s2 `intersect` intersect s1 s2 == []


-- testing (union . intersect) == union
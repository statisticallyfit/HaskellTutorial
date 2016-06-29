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

-- note returns elements of s1 which do not belong to s2.
diff :: Ord a => Set a -> Set a -> Set a
diff (Set xs) (Set ys) = Set ans
    where occs = numOccursAll ys xs
          ansPairs = zip xs occs
          ansZeroes = filter (\(x, occ) -> occ == 0) ansPairs
          ans = map fst ansZeroes



--- exercise 37 ------------------------------------------------------------------------

-- note returns elements which do not lie in either of the sets.
symmetricDiff :: Ord a => Set a -> Set a -> Set a
symmetricDiff s1 s2 = Set (eliminateFrom commons alls)
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


symmetricDiff' :: Ord a => Set a -> Set a -> Set a
symmetricDiff' s1 s2 = diff s1 s2 `union` diff s2 s1


--- exercise 38 ------------------------------------------------------------------------

-- note from graham hutton book
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = yss ++ map (x:) yss
    where yss = subsets xs



powerSet :: Ord a => Set a -> Set (Set a )
powerSet (Set xs) = Set (map (Set $) (subsets xs))

-- HELP to implement powerSet using just the Set API.
{-
foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr f s []     = s
foldr f s (x:xs) = f x (foldr f s xs)
-}
{-
foldSet :: (a -> b -> b) -> b -> Set a -> b
foldSet f s (Set xs) = foldr f s xs -- note s == seed


--nonEmpties (Set []) = Set []
nonEmpties (Set (x:xs)) = foldSet f seed (nonEmpties (Set xs))
    where f ys r = ys : (x : ys) : r
          seed = Set []
-}



--- exercise 39 ------------------------------------------------------------------------

-- when given one set just return it
-- when given set of many sets, then get union of them.

setUnion :: Ord a => Set (Set a) -> Set a
setUnion (Set [s]) = s
setUnion (Set (s1 : s2 : ss)) = setUnion (Set (uniSet : ss))
    where uniSet = union s1 s2

setUnionFold :: Ord a => Set (Set a) -> Set a
setUnionFold (Set ss) = foldSet union (Set []) (Set ss)


setInter :: Ord a => Set (Set a) -> Set a
setInter (Set ss) = foldSet intersect seed (Set ss)
    where seed = setUnion (Set ss)





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

-- note returns list of num occurs of list (elm : elems) in list xs
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


---------------------------------------------------------------
-- testing unique . sorted . makeSet
testMake :: Ord a => [a] -> Bool
testMake xs = sorted xs' && unique xs'
    where Set xs' = makeSet xs

-- testing memSet == elem
testMember :: Int -> [Int] -> Bool
testMember n xs  = elem n xs' == memSet (Set xs') n
    where (Set xs') = makeSet xs

-- note for the below 3 tests, it doesn't matter if elements are duplicated or not in order.
-- testing intersect xs ys == intersect ys xs
testIntersectID :: Ord a => Set a -> Set a -> Bool
testIntersectID s1 s2 = intersect s1 s2 == intersect s2 s1

-- testing union xs ys == union ys xs
testUnionID :: Ord a => Set a -> Set a -> Bool
testUnionID s1 s2 = union s1 s2 == union s2 s1


-- testing diff s1 s2 == list1 \\ list2
testDiffEqualsLibraryDiff :: [Int] -> [Int] -> Bool
testDiffEqualsLibraryDiff xs ys = dSet == (ls1 \\ ls2)
    where s1@(Set ls1) = makeSet xs
          s2@(Set ls2) = makeSet ys
          Set dSet = diff s1 s2

-- testing symdiff s1 s2 == symdiff s2 s1
testSymDiffID :: Ord a => Set a -> Set a -> Bool
testSymDiffID s1 s2 = symmetricDiff s1 s2 == symmetricDiff s2 s1

-- testing other symDiff
testSymDiffEqualsSymDiffDefinedWithDiff :: [Int] -> [Int] -> Bool
testSymDiffEqualsSymDiffDefinedWithDiff xs ys = symmetricDiff s1 s2
                                                == symmetricDiff' s1 s2
    where s1 = makeSet xs
          s2 = makeSet ys

-- note here it matters that elements are not duplicated and sorted.
-- testing diff s1 s2 `intersect` intersect s1 s2 == []
testSymDiffIntersect :: [Int] -> [Int] -> Bool
testSymDiffIntersect xs ys = ((symmetricDiff s1 s2) `intersect` (intersect s1 s2))
                                == (Set [])
    where s1 = makeSet xs
          s2 = makeSet ys

-- testing (union . intersect) == union
testUnionIntersect :: [Int] -> [Int] -> Bool
testUnionIntersect xs ys = (union theUnion theInter) == theUnion
    where s1 = makeSet xs
          s2 = makeSet ys
          theUnion = union s1 s2
          theInter = intersect s1 s2

-- testing setIntersect. THe result must be present in each list.
-- HELP how to generate all those random lists?
-- testSetIntersect :: [Int] -> [Int]

-- testing setUnion. Each individual list must be contained in the result list.
-- help same problem as above.
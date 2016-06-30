import Data.List (sort)
import Prelude hiding (traverse)
import Test.QuickCheck



-- relation relates several pairs - gives them a common part.
-- example isParent or isSibling
type Relation a = Set (a, a)
type People = String


-- HELP HELP HELP TODO is this the definition?
isParent :: Relation People
isParent = parentChildPairs

-- example (Ben, Sue) `compose` (Sue, Joe) ==> (Ben, Joe) means (Grandparent, child)
isGrandparent :: Relation People
isGrandparent = isParent `compose` isParent

isSibling :: Relation People
isSibling = siblingPairs

---------------------------------------------------
-- note gets the snd pair element from the given first pair element using the relation.
-- example if we use rel == isParent and val == "George" it returns his child "Michael".
image :: Ord a => Relation a -> a -> Set a
image rel val = mapSet snd (filterSet ((== val) . fst) rel)

-- note returns result of image just applied over a whole set of names. Then unions them.
setImage :: Ord a => Relation a -> Set a -> Set a
setImage rel {-set arg here-} = unionSet . mapSet (image rel) -- set arg here

-- note: takes union of a set.
unionSet :: Ord a => Set (Set a) -> Set a
unionSet = foldSet union empty -- set of set arg here

-- example: addImage famRel sfam = Set ["Joe","Ben", "Sue"]
addImage :: Ord a => Relation a -> Set a -> Set a
addImage rel set = set `union` setImage rel set

-- note returns the snd of pairs from s1 (children) since fsts are parents of snds.
-- Then unions them with the original set s1.
addChildren :: Set People -> Set People
addChildren = addImage isParent

-- note generates all pair combos of pairs and if the inner are equal, then returns
-- the pair of pair's outer elements.
compose :: Ord a => Relation a -> Relation a -> Relation a
compose rel1 rel2
    = mapSet outer (filterSet equals (setProduct rel1 rel2))
    where
    equals ((a,b), (c,d)) = b == c
    outer  ((a,b), (c,d)) = (a,d)


-- note foreach - yields combination of the two sets.
setProduct :: (Ord a, Ord b) => Set a -> Set b -> Set (a,b)
setProduct s1 s2 = unionSet (mapSet (adjoin s1) s2)

-- note adds given element snd in this set for each pair.
adjoin :: (Ord a, Ord b) => Set a -> b -> Set (a,b)
adjoin set el = mapSet (addEl el) set
    where addEl el el' = (el', el)

-- note: given a digraph G(V, E) the transitive closure
-- is a digraph G'(V', E') such that
--- > V' = V (same set of vertices)
--- > If (vi, v(i+1), ..., vk) is a path in G,
--    then (vi, vk) is an edge of E'.
-- note relation is transitive if for all (a,b) and (b,c) in the relation, (a,c)
-- is also in the relation.
-- This means adding length-one path generations.
transitiveClosure :: Ord a => Relation a -> Relation a
transitiveClosure rel = limit addGen rel
        where
        --addGen rel = rel `union` (rel `compose` rel)
        addGen rel' = rel' `union` (rel' `compose` rel) -- help todo same as above?

-- note takes first element in the sequence which equals its successor.
limit :: Eq a => (a -> a) -> a -> a
limit f x
    | x == next = x
    | otherwise = limit f next
    where
    next = f x

{-
Example evaluation:

limit (addImage famRel {Ben} ))
=> {Ben} == {Ben, Sue} --- > False

limit (addImage famRel {Ben,Sue})
=> {Ben,Sue} == {Ben,Joe,Sue} --- > False

limit (addImage famRel {Ben,Joe,Sue})
=> {Ben,Joe,Sue} == {Ben,Joe,Sue} --- > True

--- > {Ben,Joe,Sue}
-}





r1 :: Relation Integer
r1 = Set [(1,2),(2,3),(3,4)]

r2 :: Relation Integer
r2 = Set [(1,2),(1,4),(1,6),
          (2,4),(2,5),
          (3,1),(3,6),
          (4,3),(4,5),(4,6),(4,7),
          (5,7),
          (7,6)]

famRel :: Relation String
famRel = Set [("Ben", "Sue"), ("Sue", "Joe")] -- ben is father of sue, sue is mother of joe.

sfam :: Set String
sfam = Set ["Joe","Ben"]

-- note first element is parent, second is child. So Ben parent, Sue child.
parentChildPairs :: Relation String
parentChildPairs = Set [("George","Michael"),("Evangeline","Wendy"), ("Gordon", "John"),
            ("Pan","Peter")]

isSib :: Relation String
isSib = Set [("Michael","Wendy"),("Michael","John"),("Wendy", "John"),
    ("Wendy","Michael"),("John", "Michael"),("John","Wendy")]

siblingPairs :: Relation String
siblingPairs = Set [("Fabiana","Milano"), ("Adrianne","Kate"), ("Thalia","Veronique"),
                ("Sara","Berenice"),("David","Julian")]

s1 :: Set String
s1 = Set ["Gabriel", "Evangeline", "George", "Gordon", "Kathryn", "Pan", "Josh",
            "Doreen", "Fabiana", "Thalia", "Julian", "Sara"]

s2 :: Set String
s2 = Set ["Fitz", "Sophie", "Dex", "Keefe", "Biana", "Silveny", "Edaline", "Grady"]










--- TESTING ------------------------------------------------------------------------------

--- testing transitive closure: intersect (trans clos) (givenRel) == givenRel
-- (which means givenRel is contained in transitive closure)
testTC_GivenRelationsAreContainedInTransClosure :: [(Int,Int)] -> Bool
testTC_GivenRelationsAreContainedInTransClosure ps
    = givenRel == intersect tcRel givenRel
    where givenRel = makeSet ps
          tcRel = transitiveClosure givenRel


--- testing transitive closure - vertices remain the same.
testTC_VerticesDontChange :: [(Int,Int)] -> Bool
testTC_VerticesDontChange ps = oldVertices == newVertices
    where givenRel = makeSet ps
          getVertices rel = (mapSet fst rel) `union` (mapSet snd rel)
          oldVertices = getVertices givenRel
          newVertices = getVertices (transitiveClosure givenRel)


--- testing transitive closure - doing it twice is same as doing it once.
testTC_TwiceIsOnce :: [(Int,Int)] -> Bool
testTC_TwiceIsOnce ps = onceTC == twiceTC
    where givenRel = makeSet ps
          onceTC = transitiveClosure givenRel
          twiceTC = transitiveClosure onceTC


--- testing adjoin: fsts of result should equal initial set.
testAdjoin :: [Int] -> Int -> Bool
testAdjoin xs n = mapSet fst adj == givenSet
    where givenSet = makeSet xs
          adj = adjoin givenSet n


--- testing setProduct: fsts of result should equal first list. And fsts of result with
-- args swapped should equal second list.
testSetProduct :: [(Int,Int)] -> [(Int,Int)] -> Bool
testSetProduct xs ys = (mapSet fst resultNonSwap) == (mapSet snd resultSwap)
    where s1 = makeSet xs
          s2 = makeSet ys
          Set p12 = setProduct s1 s2
          Set p21 = setProduct s2 s1
          resultNonSwap@(Set r12) = makeSet p12
          resultSwap@(Set r21) = makeSet p21





















------------------------------------------------------------------------------------------

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
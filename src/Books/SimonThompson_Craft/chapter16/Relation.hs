import Data.List hiding (union, intersect)

-- relation relates several pairs - gives them a common part.
-- example isParent or isSibling
type Relation a = Set (a, a)
type People = String


-- HELP HELP HELP TODO is this the definition?
isParent :: Relation People
isParent = parents

isSibling :: Relation People
isSibling = siblings

---------------------------------------------------
-- note gets the snd pair element from the given first pair element using the relation.
image :: Ord a => Relation a -> a -> Set a
image rel val = mapSet snd (filterSet ((== val) . fst) rel)

setImage :: Ord a => Relation a -> Set a -> Set a
setImage rel = unionSet . mapSet (image rel)

unionSet :: Ord a => Set (Set a) -> Set a
unionSet = foldSet union empty -- set of set arg here

addImage :: Ord a => Relation a -> Set a -> Set a
addImage rel set = set `union` setImage rel set

addChildren :: Set People -> Set People
addChildren = addImage isParent

compose :: Ord a => Relation a -> Relation a -> Relation a
compose rel1 rel2
    = mapSet outer (filterSet equals (setProduct rel1 rel2))
    where
    equals ((a,b), (c,d)) = b == c
    outer  ((a,b), (c,d)) = (a,d)

setProduct :: (Ord a, Ord b) => Set a -> Set b -> Set (a,b)
setProduct st1 st2 = unionSet (mapSet (adjoin st1) st2)

adjoin :: (Ord a, Ord b) => Set a -> b -> Set (a,b)
adjoin set el = mapSet (addEl el) set
    where addEl el el' = (el', el)

tClosure :: Ord a => Relation a -> Relation a
tClosure rel = limit addGen rel
        where
        addGen rel' = rel' `union` (rel' `compose` rel)

limit :: Eq a => (a -> a) -> a -> a
limit f x
    | x == next = x
    | otherwise = limit f next
    where
    next = f x





parents :: Relation String
parents = Set [("Ben", "Sue"), ("Leo", "Georgette"), ("Susan", "Vincent"),
            ("Courtney", "Nick")]

siblings :: Relation String
siblings  = Set [("Fabiana","Milano"), ("Adrianne","Kate"),
                ("Sara","Berenice"),("David","Julian")]

s1 :: Set String
s1 = Set ["Brian", "Georgette", "Susan", "Carrie", "Kathryn", "Oona", "Max", "Doreen"]
































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
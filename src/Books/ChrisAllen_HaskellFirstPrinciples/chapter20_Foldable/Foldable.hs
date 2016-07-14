module Foldable where


import Data.Foldable
import Data.Monoid


{-
class Foldable (t :: * -> *) where
    note fold combines (squashes) elements inside monoidal structure.
    fold    :: Monoid m => t m      -> m
    note foldMap first maps each element of structure to a monoid and then squashes
    using mappend instance of the Monoid given.
    foldMap :: Monoid m => (a -> m) -> t a -> m
-}

sumFold = fold [1,2,3,4,5 :: Sum Integer]
prodFold = fold [1,2,3,4,5 :: Product Integer]

sumFoldMap  = foldMap Sum [1,2,3,4,5]
prodFoldMap = foldMap Product [1,2,3,4,5]
allFoldMap  = foldMap All [True, False, True]
anyFoldMap  = foldMap Any [True, False, True]
firstFoldMap = foldMap First [Just 1, Nothing, Just 5] -- these get the last Just n
lastFoldMap = foldMap Last [Just 1, Nothing, Just 5]


-- note foldMap can take a non-monoid function and use it to map to end Monoid instance.
ex1 = foldMap (*5) [1,2,3 :: Product Integer] -- 5 * 10 * 15 = 750
ex2 = foldMap (*5) [1,2,3 :: Sum Integer] -- 5 + 10 + 15 == 30


-- note if what you are folding over has 1 value then declaring Monoid instance won't
-- change final behavior because there is nothing to mappend to the one value.
ex3 = foldMap (*5) (Just 100) :: Product Integer -- 500
ex4 = foldMap (*5) (Just 5) :: Sum Integer -- 25






--- 20.3 DEMONSTRATING FOLDABLE INSTANCES ------------------------------------------------

data Identity a = Identity a deriving (Eq, Show)

-- note only need to write foldr OR foldMap -- won't work just with foldl declared.
instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x -- note no mappending needed, since just 1 value.


id1 = foldr (-) 5 (Identity (-8)) --- >  -8 - 5   = -13
id2 = foldl (-) 5 (Identity (-8)) --- >  5 - (-8) = 13
id3 = foldMap (*5) (Identity 100) :: Product Integer




-- note key explaining Maybe type. ------------------------------------------------------
data Option a = None | Some a deriving (Eq, Show)

instance Foldable Option where
    foldr _ z None = z
    foldr f z (Some x) = f x z

    foldl _ z None = z
    foldl f z (Some x) = f z x

    foldMap _ None = mempty
    foldMap f (Some x) = f x


op1 = foldr (+) 1 None
op2 = foldr (-) 2 (Some (-10)) --- >  (-10) - 2
op3 = foldl (-) 9 None
op4 = foldl (-) 8 (Some (-17)) --- >  8 - (-17)
op5 = foldMap (+1) None :: Sum Integer -- uses the mempty of Sum
op6 = foldMap (+1) None :: Product Integer -- uses the mempty of Product
op7 = foldMap (+1) (Some 2) :: Sum Integer -- uses the mappend of Sum
op8 = foldMap (+1) (Some 2) :: Product Integer -- uses the mappend of Product








--- 20.5 Some basic derived operations ---------------------------------------------------

-- note
-- toList :: t a -> [a]
list1 = toList (Just 1)
list2 = map toList [Just 1, Just 2, Just 3]
list3 = concatMap toList [Just 1, Just 2, Just 3]
list4 = concatMap toList [Just 1, Just 2, Nothing]
list5 = toList (1,2) -- because kind of tuple is *-> * -> * then one arg (first from
-- tuple) must get used up so it cannot be used by the function toList.
 -- help why error when: toList (1,2,3)



-- note
-- null :: t a -> Bool
null1 = null (Left 3)
null2 = null []
null3 = null Nothing
null4 = null (1,2)
null5 = fmap null [Just 1, Just 2, Nothing]



-- note: the outermost or leftmost type args of Maybe, Either, tuples,, are part of the
-- (t) structure, not the (a) arg  (see kind @ list5)
-- length :: t a -> Int
len1 = length (1,2)
len2 = length [(1,2), (3,4), (4,5)]
len3 = fmap length [(1,2), (3,4), (4,6)]
len4 = fmap length Just [1,2,3]
len5 = length Nothing
len6 = fmap length [Just 1, Just 2, Just 3]
len7 = fmap length [Just 1, Just 2, Nothing]

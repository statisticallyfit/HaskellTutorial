import Data.List
import Test.QuickCheck


data UniqueQueue a = UniqueQueue [a] deriving (Eq, Show)

-- note adds to end. Does not add if duplicated somewhere in the list.
-- precondition all items before the separate one must be unique. So if there
-- are duplicates, it would come from the newly added item (x).
add :: Eq a => a -> UniqueQueue a -> UniqueQueue a
add x q@(UniqueQueue xs)
    | isDifferent x xs = UniqueQueue (xs ++ [x])
    | otherwise = UniqueQueue xs -- do not add if duplicate with any other in list.
    where isDifferent x [] = True
          isDifferent x (y:ys)
            | x == y = False
            | otherwise = True && isDifferent x ys

-- note remove from front
remove :: UniqueQueue a -> UniqueQueue a
remove (UniqueQueue []) = UniqueQueue [] -- note help todo no error so that property works
remove (UniqueQueue xs) = UniqueQueue (tail xs)



q1 :: UniqueQueue Integer
q1 = UniqueQueue [1,2,3,4,5,6,7,8,9,10]





-- TESTING Functions --------------------------------------------------------------------


instance Arbitrary a => Arbitrary (UniqueQueue a) where
    arbitrary = do
        xs <- arbitrary
        return (UniqueQueue xs)


unique :: Eq a => [a] -> Bool
unique [] = True
unique (x:xs) = x `notElem` xs && unique xs

extract :: UniqueQueue a -> [a]
extract (UniqueQueue []) = []
extract (UniqueQueue xs) = xs

-- TESTING ------------------------------------------------------------------------------

-- HELP doesn't work because elements in random arbitrary list are duplicated.
-- How to specify that no elements must be duplicated, to satisfy precondition of
-- add function?
-- note no element should be duplicated before or after adding.
{-
propAdd :: Int -> UniqueQueue Int -> Bool
propAdd x q@(UniqueQueue xs) = uniqueBefore && uniqueAfter
    where uniqueBefore = unique xs
          uniqueAfter = unique $ extract $ add x q
-}


-- note check that element is added to the end.
{-
HELP same issue as above - add checks that elements when given are unique.
propAdd2 :: Int -> UniqueQueue Int -> Bool
propAdd2 x q@(UniqueQueue xs) = (last $ extract $ add x q) == x
-}

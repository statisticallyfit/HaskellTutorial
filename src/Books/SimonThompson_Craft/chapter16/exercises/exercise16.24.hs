import Prelude hiding (round)
import Test.QuickCheck
import Data.List (isPrefixOf)


data Item a = Item a deriving (Eq, Show)
data Queue a = Queue [a] deriving (Eq, Show)
data Server a = Server [Queue a] deriving (Eq, Show)


-- note add to end.
addQ :: a -> Queue a -> Queue a
addQ x (Queue xs) = Queue (xs ++ [x])

-- note add to end of a particular queue (n is zero-based index)
-- precondition n >= 0 && n < length shortest queue.
addSAt :: a -> Int -> Server a -> Server a
addSAt x n (Server qs) = Server (take n qs ++ [newQ] ++ drop (n+1) qs)
    where newQ = addQ x (qs !! n)

-- note adds to end of last queue
addS :: a -> Server a -> Server a
addS x (Server []) = Server [Queue [x]]
addS x (Server qs) = Server (init qs ++ [lastQ])
    where lastQ = addQ x (last qs)

serverSize :: Server a -> Int
serverSize (Server qs) = length qs


-- note round robin allots items to ends of each queue until item
-- list is empty - that is only when it will stop.
-- how to: cycles through list from 0 .. last index and allocates each item to the
-- ends of the oncoming queues.
-- postcondition: queues are in original order (first that was first is again first)
-- despite having cycled through all of the queues and cyclying the order in the process.
roundRobin :: [Item a] -> Server a -> Server a
roundRobin [] s = s
roundRobin items server = Server (drop n qs ++ (take n qs))
    where numQueues = serverSize server
          remainder = (length items) `mod` numQueues
          n = numQueues - remainder
          Server qs = round items server

round :: [Item a] -> Server a -> Server a
round [] s = s
round (Item n : its) (Server (q:qs)) = round its (Server (qs ++ [addQ n q]))


---------------------------------------------------
q1, q2, q3, q4, q5 :: Queue Integer
q1 = Queue [1,2,3,4,5,6,7,8,9,10]
q2 = Queue [2,4,6]
q3 = Queue [1,3,5,7]
q4 = Queue [-1,2,-3,-4,-5]
q5 = Queue [10,20,30,40,50,60,70]

s1 :: Server Integer
s1 = Server [q1, q2, q3, q4, q5]
s2 = Server [q5, q4, q3, q2, q1]

items :: [Item Integer]
items = [Item 0, Item 1, Item 2, Item 3, Item 4, Item 5, Item 6, Item 7, Item 8]

---------------------------------------------------














--- TESTING functions ------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Item a) where
    arbitrary = do
        x <- arbitrary
        return (Item x)

instance Arbitrary a => Arbitrary (Queue a) where
    arbitrary = do
        xs <- arbitrary
        return (Queue xs)

instance Arbitrary a => Arbitrary (Server a) where
    arbitrary = do
        qs <- arbitrary
        return (Server qs)


------------------------------------------------------------------------------------------


-- note check that all the queues are in same order as before (treat them as if they
-- didn't have the new items attached to their ends)
-- HELP TODO there may be some false true tests? Example: all the lists may be initially
-- the same and sure when you test prefixes result is true because they are all the
-- same and we aren't really testing if roundRobin did its job (to revert them back
-- to original order after cyclying).
propRoundRobin :: [Item Int] -> Server Int -> Bool
propRoundRobin items s@(Server oldQs) = and $ map isPrefixQs zipped
    where (Server newQs) = roundRobin items s
          isPrefixQs = (\(Queue xs, Queue ys) -> isPrefixOf xs ys)
          zipped = zip oldQs newQs



------------------------------------------------------------------------------------------

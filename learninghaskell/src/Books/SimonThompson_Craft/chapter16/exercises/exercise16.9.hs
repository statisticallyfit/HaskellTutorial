import Test.QuickCheck


data Queue a = Queue [a] deriving (Eq, Show)

empty :: Queue a
empty = Queue []

isEmpty :: Queue a -> Bool
isEmpty (Queue []) = True
isEmpty _          = False

-- note add to the end (like lineup)
add :: a -> Queue a -> Queue a
add x (Queue xs) = Queue (xs ++ [x])

-- note remove from front (like lineup getting customers)
remove :: Queue a -> (a, Queue a)
remove q@(Queue xs)
    | not (isEmpty q) = (head xs, Queue (tail xs))
    | otherwise = error "remove from front"



q1 :: Queue Integer
q1 = Queue [1,2,3,4,5,6,7,8,9,10]

q2 :: Queue String
q2 = Queue ["let's", "go", "fly", "a", "kite", "up", "to", "the", "highest", "height"]




----------------------------------------------------------------------------------------

-- note add to front
add' x (Queue xs) = Queue (x:xs)

-- note remove from last
remove' q@(Queue xs)
    | not (isEmpty q) = (last xs, Queue (init xs))
    | otherwise = error "remove' from last"




-- More efficient (but same behavior (?? - not really)) -HELP --------------------------
data QueueSplit a = QueueSplit [a] [a] deriving (Eq, Show)

emptyQS :: QueueSplit a
emptyQS = QueueSplit [] []

isEmptyQS :: QueueSplit a -> Bool
isEmptyQS (QueueSplit [] []) = True
isEmptyQS _ = False

-- note add to the head of right list
addQS :: a -> QueueSplit a -> QueueSplit a
addQS x (QueueSplit xs ys) = QueueSplit xs (x:ys)

-- note remove from head of left list
removeQS :: QueueSplit a -> (a, QueueSplit a)
removeQS (QueueSplit [] []) = error "remove QS"
removeQS (QueueSplit [] ys) = removeQS (QueueSplit (reverse ys) [])
removeQS (QueueSplit (x:xs) ys) = (x, QueueSplit xs ys)




qs1 :: QueueSplit Integer
qs1 = QueueSplit [1,2,3,4,5] [6,7,8,9,10]

qs2 :: QueueSplit String
qs2 = QueueSplit ["let's", "go", "fly", "a", "kite"]
                 ["up", "to", "the", "highest", "height"]










-- TESTING functions ---------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Queue a) where
    arbitrary = do
        xs <- arbitrary
        return (Queue xs)

instance Arbitrary a => Arbitrary (QueueSplit a) where
    arbitrary = do
        xs <- arbitrary
        ys <- arbitrary
        return (QueueSplit xs ys)


extract :: Queue a -> [a]
extract (Queue xs) = xs

extractQS :: QueueSplit a -> ([a], [a])
extractQS (QueueSplit xs ys) = (xs, ys)


-- TESTING ADT 1 ------------------------------------------------------------------------


-- note check that it adds to end: add to end, reverse and see if item is the head
propQueueAdd :: Int -> Queue Int -> Bool
propQueueAdd x q = (head $ reverse q') == x
    where Queue q' = add x q

-- note when removing from front, result list should equal tail of old list.
{-

HELP it is failing because (I think) remove returns error for empty list.


propQueueRemove :: Queue Int -> Bool
propQueueRemove q@(Queue xs) = resultQList == (tail xs)
    where resultQList = extract $ snd $ remove q
-}


-- TESTING ADT 2 -----------------------------------------------------------------------

-- note when adding to font, check that added item is head.
propQueueAdd' :: Int -> Queue Int -> Bool
propQueueAdd' x q@(Queue xs) = (head $ extract $ add' x q) == x

-- note when removing from last check that result list is init of old list.

{-

HELP it is failing because (I think) remove returns error for empty list.

propQueueRemove' :: Queue Int -> Bool
propQueueRemove' q@(Queue xs) = (snd $ extract $ remove' q) == (init xs )
-}


-- TESTING ADT 3 -----------------------------------------------------------------------


-- note if I add to head of right list, check that it is there.
propQueueAddQS :: Int -> QueueSplit Int -> Bool
propQueueAddQS x q@(QueueSplit xs ys) = (head $ snd $ extractQS $ addQS x q) == x

-- note if I remove from head of left list check that result list is tail of old one.
-- HELP same error as for the last three.
{-
propQueueRemQS :: QueueSplit Int -> Bool
propQueueRemQS q@(QueueSplit xs ys) = (snd $ extractQS $ snd $ removeQS q) == (tail xs )
-}


----------------------------------------------------------------------------------------



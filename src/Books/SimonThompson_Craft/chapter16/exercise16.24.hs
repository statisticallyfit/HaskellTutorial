
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





-- note round robin allots items to ends of each queue until item
-- list is empty - that is only when it will stop.
-- how to: cycles through list from 0 .. last index and allocates each item to the
-- ends of the oncoming queues.
roundRobin :: [Item a] -> Server a -> Server a
roundRobin [] s = s
roundRobin is@(Item n : its) s@(Server (q:qs)) = drop n result ++ (take n result)
    where result = roundRobin its (Server (qs ++ [addQ n q]))
          numQueues = length (q:qs)
          remainder = (length is) `mod` numQueues
          n = numQueues - remainder





q1, q2, q3, q4, q5 :: Queue Integer
q1 = Queue [1,2,3,4,5,6,7,8,9,10]
q2 = Queue [2,4,6]
q3 = Queue [1,3,5,7]
q4 = Queue [-1,2,-3,-4,-5]
q5 = Queue [10,20,30,40,50,60,70]

s1 :: Server Integer
s1 = Server [q1, q2, q3, q4, q5]

items :: [Item Integer]
items = [Item 0, Item 1, Item 2, Item 3, Item 4, Item 5, Item 6, Item 7, Item 8]
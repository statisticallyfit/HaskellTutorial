

-- did not make sense of instructions so am doing this:
-- NOTE: priority queue gives most weight to elements at front of line  than to the ones
-- at the back.

-- note this ADT is a tuple of priority (weight) and element
-- note first type (a) contains priority and (b) contains element itself.
data PriorityQueue a b = PriorityQueue [(a,b)] deriving (Eq, Show)

-- note checks if each tuple pair has priorities in descending order.
isValid :: (Ord a, Integral a) => PriorityQueue a b -> Bool
isValid q@(PriorityQueue ps) = areDecreasing ps
    where areDecreasing [] = True
          areDecreasing [p] = True
          areDecreasing (p1:p2:ps)
            | fst p1 >= (fst p2) = True && areDecreasing (p2:ps)
            | otherwise = False

{--- note resets if not valid
HELP gives error if (a) is not Int because of the 1..length thingy. Help?

resetPriorities :: PriorityQueue Int b -> PriorityQueue Int b
--(Ord a, Integral a) => PriorityQueue a b -> PriorityQueue a b
resetPriorities q@(PriorityQueue ps)
    | isValid q = q
    | otherwise = PriorityQueue (zip newPriorities elems)
    where newPriorities = reverse $ [1 .. length ps]
          elems = map snd ps
-}


-- note for add, add to back of line, and make priority one less than guy before it.
-- resets priorities if given queue is not valid.
add :: Integral a => b -> PriorityQueue a b -> PriorityQueue a b
add elm q@(PriorityQueue ps) = PriorityQueue (ps ++ [(priorityOfLast -1, elm)])
    where priorityOfLast = fst $ last ps


-- note for remove, remove from front, and then increment the priorities of remaining
-- elements.
remove :: Integral a => PriorityQueue a b -> PriorityQueue a b
remove (PriorityQueue []) = PriorityQueue []
remove q@(PriorityQueue ps) = PriorityQueue (map (\(p,e) -> (p+1, e)) (tail ps))



p1, p2 :: PriorityQueue Integer Char
p1 = PriorityQueue [(10,'a'), (9,'f'), (7,'b'), (4,'c'), (3,'h'), (2,'j'), (1,'k')]
p2 = PriorityQueue [(10,'a'), (10,'f'), (11,'b'), (4,'c'), (3,'h'), (2,'j'), (1,'k')]

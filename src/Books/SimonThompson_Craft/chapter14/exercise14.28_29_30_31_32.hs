import Test.QuickCheck


data Edit = Change Char
          | Copy
          | Swap -- means swap the next two
          | Delete
          | Insert Char
          | Kill
          deriving (Eq, Show)

-- note finds lowest cost sequence of edits to take us from one string to another.
-- in general case, if first two chars of strnisg are equal, then Copy. Otherwise,
-- try all possibilities and choose the best of them.
-- HELP how to make this changed function shorter?
transform :: String -> String -> [Edit]
transform [] [] = []
transform xs [] = [Kill] -- note to turn xs -> [] just kill it
transform [] ys = map Insert ys  -- note to turn [] -> ys insert ys.
transform (x:xs) (y:ys)
    | x == y = Copy : transform xs ys
    | length (x:xs) <= 2 && length (y:ys) <= 2 =
        if (a == e && b == d) then (Swap : transform cs fs) else bestChoice
        where a = x; b = head xs; cs = tail xs
              d = y; e = head ys; fs = tail ys
    | otherwise = bestChoice
    where bestChoice = best [Delete   : transform xs (y:ys),
                             Insert y : transform (x:xs) ys,
                             Change y : transform xs ys]
{-    where a  = Just x
          b  = if (length (x:xs) >= 2) then Just (head xs) else Nothing
          cs = if (length (x:xs) > 2) then Just (tail xs) else Nothing
          d  = Just y
          e  = if (length (y:ys) >= 2) then Just (head ys) else Nothing
          fs = if (length (y:ys) > 2) then Just (tail ys) else Nothing-}
{-
transform [x] [y]
    | x == y = Copy : []
    | otherwise = best [Delete   : transform [] [y],
                        Insert y : transform [x] [],
                        Change y : transform [] []]
transform [a,b] [d,e]
    | a == d = Copy : transform [b] [e]
    | a == e && b == d = Swap : []
    | otherwise = best [Delete   : transform [b] [d,e],
                        Insert d : transform [a,b] [e],
                        Change d : transform [b] [e]]
transform (a:b:cs) (d:e:fs) -- (x:xs) (y:ys)
    | a == d = Copy : transform xs ys
    | a == e && b == d = Swap : transform cs fs
    | otherwise = best [Delete   : transform xs (y:ys),
                        Insert y : transform (x:xs) ys,
                        Change y : transform xs ys]
    where (x:xs) = (a:b:cs)
          (y:ys) = (d:e:fs)
-}

best :: [[Edit]] -> [Edit]
best [es] = es
best (es : ess)
    | cost es <= cost b = es
    | otherwise = b
    where b = best ess

-- note cost is given by charging one for every operation except copy which means
-- leave unchanged
cost :: [Edit] -> Int
cost = length . filter (/= Copy)



edit :: [Edit] -> String -> String
edit _ [] = []
edit [] xs = xs
edit (Insert c : rest) (x:xs) = c : edit rest (x:xs)
edit (Change c : rest) (x:xs) = c : edit rest xs
edit (Copy : rest) (x:xs) = x : edit rest xs
edit (Swap : rest) (a:b:cs) = b: a : edit rest cs
edit (Delete : rest) (x:xs) = edit rest xs
edit (Kill : rest) xs = []




transform' :: String -> String -> [Edit]
transform' [] [] = []
transform' xs [] = [Kill] -- note to turn xs -> [] just kill it
transform' [] ys = map Insert ys  -- note to turn [] -> ys insert ys.
transform' (x:xs) (y:ys)
    | x == y = Copy : transform' xs ys
    | otherwise = best [Delete   : transform' xs (y:ys),
                        Insert y : transform' (x:xs) ys,
                        Change y : transform' xs ys]




-- Testing -- exercise 30

-- NOTE: Uses for transform function.
-- NOTE: how the transform function works.




-- HELP TODO
-- NOTE properties of transform:
-- PROP 1: cost if its operations should be no larger than cost of building target
-- string letter by letter and then killing oridinal string (cost of length ys +1)
propTransformLength :: String -> String -> Property
propTransformLength xs ys = length (xs ++ ys) <= 15 ==> -- constrained < 15 for efficiency
    cost (transform xs ys) <= length ys + 1

-- HELP TODO
-- NOTE
-- PROP 2: sequence of edits resulting should indeed take the string xs to ys
-- when it is applied
propTransform xs ys = length (xs ++ ys) <= 15 ==>
    edit (transform xs ys) xs == ys

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
transform :: String -> String -> [Edit]
transform [] [] = []
transform xs [] = [Kill] -- note to turn xs -> [] just kill it
transform [] ys = map Insert ys  -- note to turn [] -> ys insert ys.
transform (a:b:cs) (d:e:fs) -- (x:xs) (y:ys)
    | a == d = Copy : transform xs ys
    | a == e && b == d = Swap : transform cs fs
    | otherwise = best [Delete   : transform xs (y:ys),
                        Insert y : transform (x:xs) ys,
                        Change y : transform xs ys]
    where (x:xs) = (a:b:cs)
          (y:ys) = (d:e:fs)

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
edit (Delete : rest) (x:xs) = edit rest xs
edit (Kill : rest) xs = []
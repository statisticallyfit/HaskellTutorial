-- note subs returns all subsequences of a list (all combinations of excluding or
-- including each element)
subs        :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs


-- note interleave returns all ways of inserting new element into list
interleave          :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x : y : ys) : map (y:) (interleave x ys)

-- note: perms returns all permutations (reorderings)
perms        :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))


-- note: choices returns all ways of choosing zero or more elements from
-- a list in any order (is the permutations of all subsequences)
choices    :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))



-- Exericse 1
choices' :: [a] -> [[a]]
choices' xs = [zs | ys <- subs xs, zs <- perms ys]
--concat [perms s | s <- subs xs]



main = do
    print $ choices [1,2,3,4]
    print $ choices' [1,2,3,4]
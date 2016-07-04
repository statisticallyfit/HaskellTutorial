
--- 1
-- note b and c work because (*) is associative

q1 = foldr (*) 1 [1..5] -- should be 1 * 2 * 3 * 4 * 5

--a = flip (*) 1 [1..5]
b1 = foldl (flip (*)) 1 [1..5]
c1 = foldl (*) 1 [1..5]



--- 2
{-
note because flip (*) == (*) ...

foldl (flip (*)) 1 [1..3]
= foldl (flip (*)) (1 * 1) [2,3]
= foldl (flip (*)) (2 * (1*1)) [3]
= foldl (flip (*)) (3 * (2 * (1*1))) []
= (3 * (2 * (1 * 1)))
-}


--- 5

a5 = foldr (++) "" ["woot", "WOOT", "woot"]

-- note max compares them alphabetically
b5 = foldr max "" ["fear", "is", "the", "little", "death"]

c5 = foldr (&&) True [False, True]

d5 = foldr (||) True [False, True]

-- note foldl f (f acc x) xs so in the expression:
-- foldl ((++) . show) "" [1..5], the ((++) . show) acts on "" first and not the number!
-- so transform to foldr or...
e5_1 = foldr ((++) . show) "" [1..5]
e5_2 = foldl (\acc y -> acc ++ show y) "" [1..5]


-- note foldr is f x (foldr f acc xs)
-- type of const :: a -> b -> a so it would work with foldl not foldr with takes a->b->b
f5_1 = foldl const 'a' [1..5]
f5_2 = foldr (flip const) 'a' [1..5]


g5_1 = foldr (flip const) 0 "tacos"
g5_2 = foldl const 0 "tacos"

h5_1 = foldl const 0 "burritos"
h5_2 = foldr (flip const) 0 "burritos"

i5_1 = foldl const 'z' [1..5]
i5_2 = foldr (flip const) 'z' [1..5]


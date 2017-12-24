fibs' :: [Int]
fibs' = 0 : 1: genFibs 0 1

genFibs     :: Int -> Int -> [Int]
genFibs a b = (a + b) : genFibs b (a + b)


-- another way using list comprehension
fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x,y) <- zip fibs (tail fibs)]

{-
NOTE evaluation

fibs = 0 : 1

zip fibs (tail fibs)
= zip [0,1] [1]
= [(0,1)]
=> 0 + 1 = 1


fibs = 0 :1 :1

= zip fibs (tail fibs)
= zip [0,1,1] [1,1]
= [(0,1), (1,1)]
= help how does this evaluate? Is result 2? What about other tuple?
how does it get overlapped with what is already in fibs?


fibs = 0 : 1 : 1 : 2

= zip fibs (tail fibs)
= zip [0,1,1,2] [1,1,2]
= [(0,1), (1,1), (1,2)]
= help evaluate how? is result 3


fibs = 0 : 1 : 1 : 2 : 3

= zip fibs (tail fibs)
= zip [0,1,1,2,3] [1,1,2,3]
= [(0,1), (1,1), (1,2), (2,3)]
= help (5?)


....
-}
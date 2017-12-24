import Data.Array -- need for knapsack problem.



-- Example of laziness - pattern matching drives evaluation
f1 :: Maybe a -> [Maybe a]
f1 m = [m, m]

f2 :: Maybe a -> [a]
f2 Nothing  = []
f2 (Just x) = [x]

-- f1 and f2 both use their argument but f1 doesn't need to know what m
-- is, so 'm' can remain unevaluated expression (called a thunk).
-- But f2 needs to know its argument, whether it is was made with a
-- Nothing or a Just, so argument first needs to be evaluated.

 -- Lazy evaluation also means some arguments are evaluated only
 -- enough to allow a pattern match, and no more.
 -- For ex: f2 (safeHead [3^500, 49]) means f2 forces evaluation of
 -- safeHead[3^500, 49] which would evaluate to Just(3^500), but
 -- 3^500 is not evaluated.


 -- Example 2 of laziness

 -- definitions of repeat and take
 {-repeat :: a -> [a]
 repeat x = x : repeat x

 take :: Int -> [a] -> [a]
 take n _      | n <= 0 =  []
 take _ []              =  []
 take n (x:xs)          =  x : take (n-1) xs




 take 3 (repeat 7)
          { 3 <= 0 is False, so we proceed to the second clause, which
        needs to match on the second argument. So we must expand
        repeat 7 one step. }
    = take 3 (7 : repeat 7)
          { the second clause does not match but the third clause
            does. Note that (3-1) does not get evaluated yet! }
    = 7 : take (3-1) (repeat 7)
          { In order to decide on the first clause, we must test (3-1)
            <= 0 which requires evaluating (3-1). }
    = 7 : take 2 (repeat 7)
          { 2 <= 0 is False, so we must expand repeat 7 again. }
    = 7 : take 2 (7 : repeat 7)
          { The rest is similar. }
    = 7 : 7 : take (2-1) (repeat 7)
    = 7 : 7 : take 1 (repeat 7)
    = 7 : 7 : take 1 (7 : repeat 7)
    = 7 : 7 : 7 : take (1-1) (repeat 7)
    = 7 : 7 : 7 : take 0 (repeat 7)
    = 7 : 7 : 7 : []

 -}




-- Consequences of laziness: foldl vs foldl'

{-
 foldl (+) 0 [1,2,3]
    = foldl (+) (0+1) [2,3]
    = foldl (+) ((0+1)+2) [3]
    = foldl (+) (((0+1)+2)+3) []
    = (((0+1)+2)+3)
    = ((1+2)+3)
    = (3+3)
    = 6

    Problem: the accumulator builds up a big unevaluated expression
    (the thunk) and the stack takes up space



foldl' (+) 0 [1,2,3]
    = foldl' (+) (0+1) [2,3]
    = foldl' (+) 1 [2,3]
    = foldl' (+) (1+2) [3]
    = foldl' (+) 3 [3]
    = foldl' (+) (3+3) []
    = foldl' (+) 6 []
    = 6

    Better: fold' requires strictness so the accumulator is evaluated
    before proceeding.
-}


-- Knapsack problem




knapsack :: [Double] -> [Integer] -> Integer -> Double
knapsack vs ws maxW = m!(numItems - 1, maxW)
    where numItems = length vs
        m = array ((-1, 0), (numItems - 1, maxW)) $
            [((-1, w), 0) | w <- [0 .. maxW]] ++
            [((i, 0), 0)  | i <- [0 .. numItems - 1]] ++
            [((i, w), best)
                | i <- [0 .. numItems - 1]
                , w <- [1 .. maxW]
                , let best
                    | ws!!i > w = m!(i-1, w)
                    | otherwise = max (m!(i-1, w))
                                      (m!(i-1, w - ws!!i) + vs!!i)
                ]

example = knapsack [3, 4, 5, 8, 10] [2, 3, 4, 5, 9] 20

main = print example
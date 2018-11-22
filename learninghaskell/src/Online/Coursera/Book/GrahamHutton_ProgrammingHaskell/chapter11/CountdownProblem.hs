{-
NOTE
Given a sequence of numbers and a target number, attempt to construct an
expression whose value is the target, by combining one or more numbers from the
sequence using addition, subtraction, multiplication, division and parentheses.
Domain: only natural numbers

For example, suppose that we are given the sequence 1, 3, 7, 10, 25, 50,
and the target 765. Then one possible solution is given by the expression
(1 + 50) ∗ (25 − 10), as shown by the following simple calculation:
= (1 + 50) ∗ (25 − 10)
= 51 ∗ (25 − 10)
= 51 ∗ 15
= 765
-}


data Op = Add | Sub | Mul | Div deriving (Eq, Show)

valid         :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0


apply         :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y


-- Expresion can either be and INT or APPLICATION OF AN OP TO TWO ARGS
data Expr = Val Int | App Op Expr Expr deriving (Eq, Show)

e1 :: Expr
e1 = App Add (App Mul (App Sub (Val 10) (App Add (Val 1) (Val 2))) (Val 3)) (Val 7)

e2 :: Expr
e2 = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))


-- note: returns list of values in an expression.
values             :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

-- note returns overall value of an expression provided it is a natural number.
eval             :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]



-- all combinatorial functions

-- note subs returns all subsequences of a list (all combinations of excluding or
-- including each element)
subs        :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

{-
NOTE how this evaluates:

FINAL ANSWER: = [[], [3], [2], [2,3], [1], [1,3], [1,2], [1,2,3]]

subs [1,2,3]
where yss' = subs [2,3]
= yss' ++ map (1:) yss'
= [[], [3], [2], [2,3]] ++ map (1:) [[], [3], [2], [2,3]]
= [[], [3], [2], [2,3], [1], [1,3], [1,2], [1,2,3]]

subs [2,3]
where yss'' = subs [3]
= yss'' ++ map (2:) yss''
= [[], [3]] ++ map (2:) [[], [3]]
= [[], [3], [2], [2,3]]

subs [3]
where yss''' = subs [] = [[]]
= yss''' ++ map (3:) yss'''
= [[]] ++ map (3:) [[]]
= [[], [3]]

(from here trace back up)
-}


-- note interleave returns all ways of inserting new element into list
interleave          :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x : y : ys) : map (y:) (interleave x ys)

-- note: perms returns all permutations (reorderings)
perms        :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

{-
NOTE how this evaluates:
1. first resolve perms before any sort of interleaving, in each case
2. then work way back up, interleaving.


perms [1,2,3,4]
= concat (map (interleave 1) (perms [2,3,4]))
= concat (map (interleave 1) [[2,3,4],[3,2,4],[3,4,2],[2,4,3],[4,2,3],[4,3,2]] )
= concat ( [ interleave 1 [2,3,4], interleave 1 [3,2,4], ... ] )
= concat [[[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]],[[1,3,2,4],[3,1,2,4],[3,2,1,4],
[3,2,4,1]],[[1,3,4,2],[3,1,4,2],[3,4,1,2],[3,4,2,1]],[[1,2,4,3],[2,1,4,3],
[2,4,1,3],[2,4,3,1]],[[1,4,2,3],[4,1,2,3],[4,2,1,3],[4,2,3,1]],[[1,4,3,2],
[4,1,3,2],[4,3,1,2],[4,3,2,1]]]

= [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1],[1,3,2,4],[3,1,2,4],[3,2,1,4],
[3,2,4,1],[1,3,4,2],[3,1,4,2],[3,4,1,2],[3,4,2,1],[1,2,4,3],[2,1,4,3],[2,4,1,3],
[2,4,3,1],[1,4,2,3],[4,1,2,3],[4,2,1,3],[4,2,3,1],[1,4,3,2],[4,1,3,2],[4,3,1,2],
[4,3,2,1]]



perms [2,3,4]
= concat (map (interleave 2) (perms [3,4]))
= concat (map (interleave 2) [[3,4], [4,3]] )
= concat ([ interleave 2 [3,4] , interleave 2 [4,3]])
= concat [[[2,3,4],[3,2,4],[3,4,2]],[[2,4,3],[4,2,3],[4,3,2]]]
= [[2,3,4],[3,2,4],[3,4,2],[2,4,3],[4,2,3],[4,3,2]]


perms [3,4]
= concat (map (interleave 3) (perms [4]))
= concat (map (interleave 3) [[4]] )
= concat ( [ interleave 3 [4]])
= [[[3,4], [4,3]]]
= [[3,4], [4,3]]


perms [4]
= concat (map (interleave 4) (perms []))
= concat (map (interleave 4) [[]] )
= concat ( [ interleave 4 [] ])
= concat [[4]]
= [4]

note from now on work way back up.

-}



-- note: choices returns all ways of choosing zero or more elements from
-- a list in any order (is the permutations of all subsequences)
choices    :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))

-- note Given a list of choices: an expression is a solution if the list of
-- expression values is chosen from the given choice list and the expression
-- evaluates to give the target.
solution             :: Expr -> [Int] -> Int -> Bool
solution e ns target = elem (values e) (choices ns) && eval e == [target]

{-
NOTE
elem [1,2] [[1],[2]]
False

elem [1,2] [[1,2]]
True

elem [1,2] [[2,1]] -- note this is why we need to generate all permutations.
False
-}















-- 11.3 BRUTE FORCE SOLUTION ----------------------------------------------------

-- note: returns all ways of splitting a list into two non-empty lists that
-- append to give original list.
split        :: [a] -> [([a], [a])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

{-
NOTE evaluation

split [1,2,3,4]
= ([1], [2,3,4]) : [(1:ls, rs) | (ls, rs) <- split [2,3,4]]
= ([1], [2,3,4]) : [(1:ls, rs) | (ls, rs) <- [([2],[3,4]),([2,3],[4])] ]
note there are two tuples so they each take their turn:
note tuple 1
([2],[3,4])
=  : [(1 : [2], [3,4] ) ]
=  : [([1,2], [3,4])]
note tuple 2
([2,3],[4])
=  : [(1 : [2,3], [4] ) ]
=  : [ ([1,2,3], [4]) ]

answer:
= [ ([1], [2,3,4]), ([1,2], [3,4]), ([1,2,3], [4]) ]



split [2,3,4]
= ([2], [3,4]) : [(2:ls, rs) | (ls, rs) <- split [3,4]]
= ([2], [3,4]) : [(2:ls, rs) | (ls, rs) <- [([3], [4])] ]
= ([2], [3,4]) : [(2 : [3], [4])]
= [([2],[3,4]),([2,3],[4])]


split [3,4]
= ([3], [4]) : [(3:ls, rs) | (ls, rs) <- split [4]]
= ([3], [4]) : []
= [([3], [4])]

split [4]
= []


note from now on move back up
-}


-- note: returns all possible expressions whose list of values is precisely a
-- given list
-- important: produce all splittings of the list then recursively
-- calculate all possible expressions for each of these lists, Then combine
-- each pair of expressions using each of the four numeric operators.
exprs     :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls, rs) <- split ns,
                        l <- exprs ls,
                        r <- exprs rs,
                        e <- combine l r]

combine     :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

{-
NOTE example of combine:

combine (App Add (Val 1) (Val 3)) (Val 2)

[App Add (App Add (Val 1) (Val 3)) (Val 2),
 App Sub (App Add (Val 1) (Val 3)) (Val 2),
 App Mul (App Add (Val 1) (Val 3)) (Val 2),
 App Div (App Add (Val 1) (Val 3)) (Val 2)]

-}

-- note: Given a list of choice numbers, make all possible expressions.
-- first generate all expressions over each choice then select those
-- expressions that equal the target.
solutions           :: [Int] -> Int -> [Expr]
solutions ns target = [e | ns' <- choices ns,
                      e <- exprs ns',
                      eval e == [target]]

















-- 11.4 COMBINING GENERATION AND EVALUATION ------------------------------------
-- since most solutions are invalid
type Result = (Expr, Int)

results     :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls, rs) <- split ns,
                            lx <- results ls,
                            ry <- results rs,
                            res <- combine' lx ry]


combine'             :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops,
                                                   valid o x y]
{-
NOTE
combine' (Val 1, 1) (Val 2, 2)

[(App Add (Val 1) (Val 2),3),(App Mul (Val 1) (Val 2),2)]


combine' (Val 2, 2) (Val 1, 1)

[(App Add (Val 2) (Val 1),3),
 (App Sub (Val 2) (Val 1),1),
 (App Mul (Val 2) (Val 1),2),
 (App Div (Val 2) (Val 1),2)]

-}

-- note: new improvement: rejects early the solutions that fail to evaluate.
 -- help understand better (go through the generated answ.ers).
solutions'           :: [Int] -> Int -> [Expr]
solutions' ns target = [e | ns' <- choices ns,
                           (e, m) <- results ns',
                           m == target]
-- note: solutions and solutions' return same number of results = 780

-- HELP mull all these functions over to fully understand (parts 11.3, 11.4)



















-- 11.5 EXPLOITING ALGEBRAIC PROPERTIES -----------------------------------------

{-
NOTE properties to exploit:

x + y = y + x
x ∗ y = y ∗ x
x ∗ 1 = x
1 ∗ y = y
x ÷ 1 = x

-}

{-
note
For example, using this new deﬁnition, Add 3 2 is now invalid because it is
essentially the same as
Add 2 3 using the commutativity of addition, while
Div 2 1 is now invalid because it is essentially the same as the number 2 on its
own using the identity property for division.
-}
valid'         :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0



combine''             :: Result -> Result -> [Result]
combine'' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops,
                                                   valid' o x y]

results'     :: [Int] -> [Result]
results' []  = []
results' [n] = [(Val n, n) | n > 0]
results' ns  = [res | (ls, rs) <- split ns,
                            lx <- results' ls,
                            ry <- results' rs,
                            res <- combine'' lx ry]

-- note: improvement: reduces duplicate expressions (and as before, invalid ones)
solutions''           :: [Int] -> Int -> [Expr]
solutions'' ns target = [e | ns' <- choices ns,
                           (e, m) <- results' ns',
                           m == target]

-- note: length of solutions: 49 (less since invalid ones or same ones are
-- eliminated.
-- HELP check if invalid ones are eliminated here or in solutions'
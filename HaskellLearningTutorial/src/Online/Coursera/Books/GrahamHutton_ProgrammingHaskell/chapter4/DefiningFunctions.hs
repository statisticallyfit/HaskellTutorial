import Prelude hiding (abs, signum, not, and)

-- Ways to structure expressions

-- 1. Conditional expressions (if-else) ++++++++++++++++++++++++++++++
abs :: Int -> Int
abs n = if n >= 0 then n else -n

signum :: Int -> Int
signum n = if n < 0 then -1 else
                if n == 0 then 0 else 1


-- 2. Guarded equations ++++++++++++++++++++++++++++++++++++++++++++++
abs' :: Int -> Int
abs' n
    | n >= 0    = n
    | otherwise = -n


-- 3. Pattern matching +++++++++++++++++++++++++++++++++++++++++++++++
-- 3.a) normal pattern matching --------------------------------------
not :: Bool -> Bool
not False = True
not True = False

-- 3.b) wildcard pattern ---------------------------------------------
and :: Bool -> Bool -> Bool
and True True = True
and _ _       = False
-- or
and' :: Bool -> Bool -> Bool
and' True b  = b
and' False _ = False
-- note:the same argument name can't be used for more than
-- note: one argument in an equation.
-- b and b = b -- not right
-- _ and _ = False
-- instead, do:
and'' :: Bool -> Bool -> Bool
and'' b c | b == c    = b
          | otherwise = False


-- 3.c) tuple patterns ------------------------------------------------
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- 3.d) list patterns -------------------------------------------------
containsThreeHeadA :: [Char] -> Bool
containsThreeHeadA ['a', _, _] = True
containsThreeHeadA _           = False

-- 3.e) integer patterns ---------------------------------------------
predecessor :: Int -> Int
predecessor n = n - 1

-- help: but pred is defined with (n+1) pattern - how to do this here?





-- 4. Lambda expressions +++++++++++++++++++++++++++++++++++++++++++++
-- help: todo: what does this signature mean for add? Why error otherwise?
--add :: Num a => t -> t1 -> a -> a -> a
add x y = \x -> (\y -> x + y)

const    :: a -> b -> a
const x _ = x

-- help: todo: how is const used?
-- using lambda makes it explicit that const returns a function
const'   :: a -> (b -> a)
const' x  = \_ -> x -- the \_ means lambda takes any argument


-- lambda is used when function is referenced only once
odds   :: Int -> [Int]
odds n = map f [0.. n-1]
            where f x = x*2 + 1
-- returns the first n odd numbers
odds'   :: Int -> [Int]
odds' n = map (\x -> x*2 + 1) [0 .. n-1]



--5. Sections ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- example if * is an operator then expressions of the form (*),
-- (x *), and (* y) are called sections
-- useful for
-- 5.a) constructing functions compactiy ----------------------------
-- (1 +) is successor function \y -> 1 + y

-- 5.b) stating types of operators ----------------------------------
-- (and) :: Bool -> Bool -> Bool

-- 5.c) when using operators as arguments to other functions ---------
--AND :: [Bool] -> Bool
--AND = foldr (&&) True
-- help: todo: why error?


main = do
    print (abs (-9))
    print (signum (-10), signum 8)
    print (and True False)
    print (and' True True, and' False True, and' True False)
    print (and'' True True, and'' False True, and'' True False)
    print (swap (1, 2))
    print (containsThreeHeadA ['a', 'b', 'c'], containsThreeHeadA ['a'])
    print (predecessor (-1), predecessor 0, predecessor 2)
    print (odds' 3)
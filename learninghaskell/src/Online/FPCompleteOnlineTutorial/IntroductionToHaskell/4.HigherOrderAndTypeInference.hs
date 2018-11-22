
-- Anonymous Functions
greaterThanOneHundred :: [Integer] -> [Integer]
greaterThanOneHundred xs = filter (>100)  xs -- filter(\x -> x > 100) xs
-- \x means lambda takes input x and ouptuts Bool (is x > 100 or not)
-- todo: why doesn't it work to declare a function composition here?




-- function composition example
-- todo: why cannot put argument explicitly for greaterthanonehundred function?
myTest :: [Integer] -> Bool
myTest = even . length . greaterThanOneHundred



-- functions only have one argument:
-- Read the following as: f takes on Int and ouputs a function of type Int -> Int
funcOneArg :: Int -> (Int -> Int)  -- same as without parantheses (func arrows associate to the right)
funcOneArg x y = 2*x + y

-- this has two args
funcTwoArgs :: (Int, Int) -> Int
funcTwoArgs (x, y) = 2*x + y

funcTwoArgs' :: (Int, Int) -> Int
funcTwoArgs' (x, y) = 3*x + y


--The “multi-argument” lambda abstraction
-- \x y z -> ...
-- is really just syntax sugar for
-- \x -> (\y -> (\z -> ...))
-- Likewise, the function definition
-- f x y z = ...
-- is syntax sugar for
-- f = \x -> (\y -> (\z -> ...))


--- Currying


-- ex1
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x) -- todo: why do you have to use the lambda? Can do without it?

compose' :: (b -> c) -> (a -> b) -> a -> c
compose' f g x = f (g x)
-- todo: why does removing lambda and putting x in compose f g x  also mean
-- todo: that the parantheses around a-> c should be removed?

-- ex2
-- todo: if this is currying, then the funcTwoArgs should take two args at a time
-- todo: shouldn't it? Why does it say if curried then uncurry? since this is supposed
-- todo: to curry, not uncurry... ????
curryFunc :: ((a, b) -> c) -> a -> b -> c -- the last part means taking one arg at a time
curryFunc f x y = f (x, y)


uncurryFunc :: (a -> b -> c) -> (a, b) -> c
uncurryFunc f(x, y) = f x y



-- EXAMPLE OF POINT-FREE STYLE
poorFoobar :: [Integer] -> Integer
poorFoobar []        = 0
poorFoobar (x:xs)
    | x > 3     = (7*x + 2) + poorFoobar xs
    | otherwise = poorFoobar xs


betterFoobar :: [Integer] -> Integer
betterFoobar = sum . map(\x -> 7*x + 2) . filter (>3)
-- todo: why doesn't map(7*_ + 2) work also?


main = do

--print ((funcOneArg 1) 2)
--print (betterFoobar [1,2,3,4,5])
    --print (uncurryFunc funcTwoArgs 2 3 ) -- todo: why error???
   print (curryFunc funcTwoArgs 2 3)
   print (curryFunc funcTwoArgs' 2 3)
--print (funcTwoArgs (2, 3))
--print (funcOneArg 3 12) -- is the same as (funcOneArg 3) 12
   print (myTest [1,9,349,6,907,98, 105])
--print (greaterThanOneHundred [1, 9, 349, 6, 907, 98, 105])
--print (filter (> 5) (filter even [1, 2, 3, 4, 5, 6, 7, 8, 9])  )

--print ((\x y z -> [x, 2*y, 3*z]) 5 6 3) --lambdas can have multiple args


module Online.Coursera.Course.Quiz8 where

-- 1

putStr' :: String -> IO()
putStr' [] = return()
putStr' (x:xs) = putChar x >> putStr' xs



-- 2

-- (>>=) :: m a -> (a -> m b) -> m b

putStrLn1 :: String -> IO ()
putStrLn1 [] = putChar '\n'
putStrLn1 xs = putStr' xs >> putStrLn1 ""

putStrLn2 :: String -> IO ()
--putStrLn2 [] = putChar '\n' -- note don't need this line since not recursive.
putStrLn2 xs = putStr' xs >> putChar '\n'

putStrLn3 :: String -> IO ()
--putStrLn3 [] = putChar '\n'  --note don't need this either since not recursive.
putStrLn3 xs = putStr' xs >>= \x -> putChar '\n'

-- note HELP so using the IO monad >>= operator here?
-- HElp don't understand how putStr' xs fulfills the first (m a) of >>= signature.



--putStrLn4 :: String -> IO ()
--putStrLn4 [] = return ()
--putStrLn4 xs = putStr' xs >> putStr' "\n"

putStrLn4 :: String -> IO ()
putStrLn4 [] = putChar '\n'
putStrLn4 xs = putStr' xs >> putStr "\n"


{-
uncover
main = do
    putStr' "hi there\n"
    print "1: "; putStrLn1 "with line break"
    print "2: "; putStrLn2 "with line break"
    print "3: "; putStrLn3 "with line break"
    print "4: "; putStrLn4 "with line break"
-}













-- 3
getLine' :: IO String
getLine' = get []

get    :: String -> IO String
get xs = do x <- getChar
            case x of
                '\n'  -> return (xs ++ "\n")
                _    -> get (xs ++ [x])

{-
uncover
main = do
    getLine'
-}








-- 4
interact'   :: (String -> String) -> IO()
interact' f = do input <- getLine
                 putStrLn (f input)







-- 5
-- find all possible implementations of
-- sequence_' :: Monad m => [m a] -> m ()
-- note this evaluates monadic values from left to right, ignoring intermediate
-- results.
-- note summary key idea: this function just returns the structure. So a list
-- of Maybes would return Nothing if it is present, and Just () if no Nothings.

-- (>>)     :: Monad m => m a -> m b -> m b
-- x >> y   =  x >>= \_ -> y  -- HELP understand this better.

-- summary of how this works:
{-
= foldl (>>) (Just 1) [Just 2, Just 3]
= foldl (Just 1) >> (Just 2) [Just 3]
= foldl (>>) (Just 2) [Just 3]
= (Just 2) >> (Just 3)
= (Just 3)
-- note it only needs to go through the whole list to make sure that the structure
-- is the only kind of structure there. For example, it might find a Nothing
-- and then that's what would be returned.

and now

(Just 3) >> return ()
= Just ()

-}


seq2        :: Monad m => [m a] -> m ()
seq2 []     = return () -- note doesn't need this case
seq2 (m:ms) = (foldl (>>) m ms) >> return ()
-- note:
-- in foldl, >> is function, m is the seed, ms is the args


-- seq3 ms = foldl (>>) (return ()) ms


seq4        :: Monad m => [m a] -> m ()
seq4 []     = return ()
seq4 (m:ms) = m >> seq4 ms


seq5        :: Monad m => [m a] -> m ()
seq5 []     = return ()
seq5 (m:ms) = m >>= \ _ -> seq5 ms -- note: works since >>= needs a function



-- seq6 ms = foldr (>>=) (return ()) ms -- HELP understand eror better

seq7    :: (Monad m, Foldable t) => t (m a) -> m ()
seq7 ms = foldr (>>) (return ()) ms


 --seq8 ms = foldr (>>) (return []) ms -- note returns Just []


















 -- 6
-- note write:
-- sequence :: Monad m => [m a] -> m [a]

s1         :: Monad m => [m a] -> m [a]
s1 []      = return []
s1 (m :ms) = m >>= \ a -> do as <- s1 ms
                             return (a : as)

{-
NOTE explanation of how this works;

s1 [Just 1, Just 2, Just 3]
= (Just 1) >>= (s1 [Just 2, Just 3])
= (Just 1) >>= ((Just 2) >>= s1 [Just 3])
= (Just 1) >>= ((Just 2) >>= ((Just 3) >>= s1 [])
= (Just 1) >>= ((Just 2) >>= ((Just 3) >>= return [])  -- help is this right?
= Just [1,2,3] -- help how exactly does it get to this step?
-}



-- HELP how is this evaluated? understand better
s5 ms = foldr func (return []) ms
        where
            func :: (Monad m) => m a -> m [a] -> m [a]
            func m acc = do
                            x <- m
                            xs <- acc
                            return (x:xs)


{-

s6 [] = return []
s6 (m:ms) = m >> \ a -> do as <- s6 ms
                           return (a : as)
-} -- note didn't work since >> cannot accept a function for second argument





{-

s7 [] = return []
s7 (m:ms) = m >>= \a ->
                    as <- s7 ms
                    return (a:as)
-} -- help why didn't this work?


sequence'        :: Monad m => [m a] -> m [a]
sequence' []     = return []
sequence' (m:ms) = do a <- m
                      as <- sequence' ms
                      return (a:as)


















-- 7

{-
note choose all possible implementations of

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]


sequence'        :: Monad m => [m a] -> m [a]
sequence' []     = return []
sequence' (m:ms) = do a <- m
                      as <- sequence' ms
                      return (a:as)
-}


g x = return x :: Maybe Int


m1      :: Monad m => (a -> m b) -> [a] -> m [b]
m1 f as = sequence' (map f as)

{-
NOTE
f x = return x :: Maybe Int

m1 f [1,2,3]
= sequence' (map f [1,2,3])
= sequence' [Just 1, Just 2, Just 3]
= Just [1,2,3]
-}

-- HELP understand better how this is evaluated
m2            :: Monad m => (a -> m b) -> [a] -> m [b]
m2 f []       = return []
m2 f (a : as) = f a >>= \ b -> m2 f as >>= \ bs -> return (b:bs)
-- HELP how is this evaluated? How does it not return [Just 1, Just 2...]?




--m4 f [] = return []
--m4 f (a:as) = f a >> \ b -> m4 f as >> \ bs -> return (b:bs)


{-
m5 f [] = return []
m5 f (a:as) = do
    f a -> b
    m5 f as -> bs
    return (b:bs)
-}

m6          :: Monad m => (a -> m b) -> [a] -> m [b]
m6 f []     = return []
m6 f (a:as) = do b <- f a
                 bs <- m6 f as
                 return (b:bs)
-- HELP how is this evaluated? How does it not return [Just 1, Just 2...]?



m7 f [] = return []
m7 f (a:as) = f a >>= \ b -> do bs <- m7 f as
                                return (b:bs)
-- help where is the function (a -> m b)?

{-

m8 f [] = return []
m8 f (a:as) = f a >>= \ b -> do bs <- m8 f as
                                return (bs ++ [b])
-}













-- 8
{-
NOTE implement
note: takes a predicate to filter a finite list of type a and if they meet
the predicate, then return the structure containing a list of the a's.

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
-}


evenM x = if even x then (Just True) else (Just False)




filterM _ [] = return []
filterM p (x:xs) = do flag <- p x -- note help how does flag end up Bool not Maybe Bool?
                      ys <- filterM p xs
                      if flag then return (x:ys) else return ys

{-
uncover
main = print $ filterM evenM [1..10]
-}






-- 9
{-
NOTE implement
foldLeftM :: Monad => (a -> b -> m a) -> a -> [b] -> m a

note: takes an accumulation function (a -> b -> m a) and a seed of type 'a' and
left folds a finite list [b] into a single result of type 'm a'
-}

sumM     :: Integer -> Integer -> Maybe Integer
sumM x y = return (x + y) -- Just(x + y)


foldLeftM            :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a []     = return a
foldLeftM f a (b:bs) = do a' <- f a b
                          foldLeftM f a' bs -- help somehow (f a b) = Just 1 but then
                                            -- a' ends up just being (1) so that
                                            -- foldLeft f 1 [2,3,4,5] can continue
                                            -- and then the list becomes Just(added)
-- HELP what monad instance is this do using? what does this mean in >>= n
-- notation?
-- (Just a) >>= f = f a


{- NOTE
e1 >>= λv1 →
e2 >>= λv2 →
en >>= λvn →
return (f v1 v2 ... vn)

to be written as:

do v1 ← e1
   v2 ← e2
   vn ← en
   return (f v1 v2 ... vn)
-}

{-
uncover
main = do
    print $ foldLeftM sumM 0 [1,2,3,4,5]
    foldLeftM (\a b -> putChar b >> return (b : a ++ [b]))
        [] "haskell" >>= \r -> putStrLn r
    -- HELP meaning of above?
-}







-- 10
foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f b [] = return b
foldRightM f b (a:as) = do b' <- foldRightM f b as
                           f a b'
{-
NOTE

foldRightM sumM 0 [1,2,3,4,5]
= 1 + (foldRightM sumM 0 [2,3,4,5])
= 1 + (2 + foldRightM sumM 0 [3,4,5])
= 1 + (2 + (3 + foldRightM sumM 0 [4,5]))
= 1 + (2 + (3 + (4 + foldRightM sumM 0 [5])))
= 1 + (2 + (3 + (4 + (5 + foldRightM sumM 0 [])
= 1 + (2 + (3 + (4 + (5 + return 0)
= 1 + (2 + (3 + (4 + (5 + Just 0)
= 1 + (2 + (3 + (4 + (5 + return 0) -- with the b' <- ... it somehow becomes..
= 1 + (2 + (3 + (4 + (5 + 0)) -- 5 before 0 since it's (f a b') not (f b' a)
= Just 15

HELP how does it end up with structure again? Help to understand
-}

{-
uncover
main =
    foldRightM (\a b -> putChar a >> return (a:b))
        [] (show [1,3..10]) >>= \r -> putStrLn r
         -- help understand result
-}














-- 11

{-
NOTE choose all possible implementations defining

liftM :: Monad m => (a -> b) -> m a -> m b

takes function (a -> b) and maps it over a monadic type (m a) to produce
type m b
-}


--aTob :: a -> b
aTob a = (+1) a


-- (>>=) :: m a -> (a -> m b) -> m b
liftM0 :: Monad m => (a -> b) -> m a -> m b
liftM0 f ma = fmap f ma


liftM1 f ma = do x <- ma
                 return (f x)

--liftM_2      :: Monad m => (a -> b) -> m a -> m b
--liftM_2 f ma = ma >>= \a -> f a
-- HELP meaning of the space between \ and a:  (\ a)?

liftM_3 f ma = ma >>= \a -> return (f a)

--liftM_4 f ma = return (f ma)


liftM_5 f ma = ma >>= \a -> ma >>= \b -> return (f a)
-- HElp meaning of the extra 'b'? how is this different from liftM_3?


liftM6 f ma = ma >>= \a -> ma >>= \b -> return (f b)
-- hELP how is this different from liftM_5?


{-
note
:t mapM
(a -> m b) -> t a -> m (t b)
-}

-- note type here is: (a -> m b) -> a -> m [b]
--liftM7 f ma = mapM f [ma]
-- output: liftM7 (\x -> Just x) 1
--       = Just [1]


liftM8 f ma = ma >> \a -> return (f a)

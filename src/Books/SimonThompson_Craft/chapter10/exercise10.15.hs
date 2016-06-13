import Prelude hiding (unzip, last, init)


-- 1 unzip ----------------------------------------------------------------------
-- method 1 ------------------------------------------
unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' xs = (foldr first [] xs, foldr second [] xs)
     where first pair zs = [fst pair] ++ zs
           second pair zs = [snd pair] ++ zs

{-
NOTE EVALUATION

foldr first [] [(1,'a'), (2,'b'), (3,'c')]
= first (1, 'a') (foldr first [] [(2,'b'), (3,'c')])
= 1 : (first (2,'b') (foldr first [] [(3,'c')])
= 1 : 2 : (foldr first [] [(3,'c')])
= 1 : 2 : (first (3,'c') (foldr first [] [])
= 1 : 2 : 3 : []
= [1,2,3]

-}
-- method 2 ------------------------------------------
unzip'' :: [(a,b)] -> ([a],[b])
unzip'' xs = (unzipFirst xs, unzipSecond xs)

unzipFirst :: [(a, b)] -> [a]
unzipFirst [] = []
unzipFirst (p:ps) = first p (unzipFirst ps) -- IMPORTANTNOTE: f x (foldr f s xs)

unzipSecond :: [(a,b)] -> [b]
unzipSecond [] = []
unzipSecond (p:ps) = second p (unzipSecond ps)

first p zs = [fst p] ++ zs
second p zs = [snd p] ++ zs


-- method 3 ------------------------------------------
-- HElP
unzip :: [(a, b)] -> ([a], [b])
unzip xs = foldr (\(a,b) (as,bs) -> (a:as, b:bs)) ([], []) xs

-- note unzip xs = foldr (\(a,b) ~(as,bs) -> (a:as, b:bs)) ([], []) xs
-- the tilde means enforce lazy evaluation.




-- 2 init  -------------------------------------------------------------------------

-- method 1 ------------------------------------------
init :: [a] -> [a]
init xs = tail $ foldr shiftLastToFirstOnce [] xs
{-
NOTE EVALUATION

init [1,2,3,4,5]
= foldr shift [] [1,2,3,4,5] -- IMPORTANT: f x (foldr f s xs)
= shift 1 (foldr shift [] [2,3,4,5])
= shift 1 (shift 2 (foldr shift [] [3,4,5]))
= shift 1 (shift 2 (shift 3 (foldr shift [] [4,5])))
= shift 1 (shift 2 (shift 3 (shift 4 (foldr shift [] [5]))))
= shift 1 (shift 2 (shift 3 (shift 4 (shift 5 (foldr shift [] [])))))
= shift 1 (shift 2 (shift 3 (shift 4 (shift 5 [] )))) -- help - does it end with []?
= shift 1 (shift 2 (shift 3 (shift 4 [5] )))
= shift 1 (shift 2 (shift 3 [5,4]))
= shift 1 (shift 2 [5,3,4])
= shift 1 [5,2,3,4]
= [5,1,2,3,4]

-}
-- method 2 ------------------------------------------
init'        :: [a] -> [a]
init' []     = []
init' (x:xs) = shiftLastToFirstOnce x (init' xs) -- pattern: f x (foldr f s xs)


-- method 3 ------------------------------------------
init''        :: [a] -> [a]
init'' [x]    = []
init'' (x:xs) = x : init'' xs



shiftLastToFirstOnce :: a -> [a] -> [a]
shiftLastToFirstOnce last [] = [last]
shiftLastToFirstOnce pred (last : preds) = last : pred : preds
------------------------------------------------------------------------------------

-- method 4 ------------------------------------------
init'''                :: Eq a => [a] -> [a]
init''' (x:xs)
    | length rest == 1 = rest -- note if only one element
    | otherwise        = x : init''' (rest)-- note otherwise if tail then continue
    where rest = skipLastOnce x xs

-- needs to be the type of the foldr function (a -> b -> b), b = [a]
skipLastOnce :: Eq a => a -> [a] -> [a]
skipLastOnce z [] = []
skipLastOnce a bs = bs
{-skipLastOnce :: Eq a => a -> [a] -> [a]
skipLastOnce x y
    | length y == 1 = [x]
    | otherwise     = y-}

-- HELP HELP HELP how to do this with skiplast and foldr?
init4 xs = foldr skipLastOnce [] xs








-- 3 last -------------------------------------------------------------------------
-- method 1 ------------------------------------------
last1    :: [a] -> a
last1 xs = head $ foldr shiftLastToFirstOnce [] xs

-- method 2 ------------------------------------------
last2 :: [a] -> a
last2 xs = head $ foldr keepLastOnce [] xs

-- method 3 ------------------------------------------
-- HELP how to declare this to return just last item (non list) but at same
-- time have type signature [a] -> a not [a] -> [a] ?
--last'        :: [a] -> a
last' []    = []
last' (x:xs) = keepLastOnce x (last' xs)

-- method 4 ------------------------------------------
last''    :: [a] -> a
last'' xs = head $ reverse xs

-- note this is just like shiftLastToFirst except it ignores the rest, cares
-- just about the last (successor value)
keepLastOnce                   :: a -> [a] -> [a]
keepLastOnce lastItem []       = [lastItem]
keepLastOnce _ (successor : _) = [successor]

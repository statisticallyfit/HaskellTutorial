import Prelude hiding (unzip, last, init)


-- 1 unzip
unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' xs = (foldr first [] xs, foldr second [] xs)
     where first pair zs = [fst pair] ++ zs
           second pair zs = [snd pair] ++ zs


-- HElP
unzip :: [(a, b)] -> ([a], [b])
unzip xs = foldr (\(a,b) (as,bs) -> (a:as, b:bs)) ([], []) xs

-- note unzip xs = foldr (\(a,b) ~(as,bs) -> (a:as, b:bs)) ([], []) xs
-- the tilde means enforce lazy evaluation.




-- 2 last

-- method 1
init :: [a] -> [a]
init xs = tail $ foldr shiftLastToFirst [] xs

-- method 2
init'        :: [a] -> [a]
init' []     = []
init' (x:xs) = shiftLastToFirst x (init' xs) -- pattern: f x (foldr f s xs)

-- method 3
init''        :: [a] -> [a]
init'' [x]    = []
init'' (x:xs) = x : init'' xs



shiftLastToFirst :: a -> [a] -> [a]
shiftLastToFirst last [] = [last]
shiftLastToFirst pred (last : preds) = last : pred : preds
---------------------------------------------------------------------------------

-- method 4
init''' :: Eq a => [a] -> [a]
init''' (x:xs)
    | length rest == 1 = rest -- note if only one element
    | otherwise        = x : init''' (rest)-- note otherwise if tail then continue
    where rest = skipLast x xs

-- needs to be the type of the foldr function (a -> b -> b), b = [a]
skipLast :: Eq a => a -> [a] -> [a]
skipLast x y
    | length y == 1 = [x]
    | otherwise     = y










-- 3
last1    :: [a] -> a
last1 xs = head $ foldr shiftLastToFirst [] xs


last2 :: [a] -> a
last2 xs = head $ foldr keepLast [] xs


-- HELP how to declare this to return just last item (non list) but at same
-- time have type signature [a] -> a not [a] -> [a] ?
--last'        :: [a] -> a
last' []    = []
last' (x:xs) = keepLast x (last' xs)


last''    :: [a] -> a
last'' xs = head $ reverse xs

-- note this is just like shiftLastToFirst except it ignores the rest, cares
-- just about the last (successor value)
keepLast                   :: a -> [a] -> [a]
keepLast lastItem []       = [lastItem]
keepLast _ (successor : _) = [successor]

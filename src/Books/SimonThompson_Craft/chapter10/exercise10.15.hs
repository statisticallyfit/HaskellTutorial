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

init'        :: [a] -> [a]
init' [x]    = []
init' (x:xs) = x : init' xs




init :: [a] -> [a]
init xs = tail $ foldr shiftLastToFirst [] xs


shiftLastToFirst :: a -> [a] -> [a]
shiftLastToFirst last [] = [last]
shiftLastToFirst pred (last : preds) = last : pred : preds
---------------------------------------------------------------------------------

init'' :: Eq a => [a] -> [a]
init'' (x:xs)
    | length rest == 1 = rest -- note if only one element
    | otherwise        = x : init'' (rest)-- note otherwise if tail then continue
    where rest = skipLast x xs

-- needs to be the type of the foldr function (a -> b -> b), b = [a]
skipLast :: Eq a => a -> [a] -> [a]
skipLast x y
    | length y == 1 = [x]
    | otherwise     = y
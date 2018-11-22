import Prelude hiding ((*), (++), product, reverse, insert, zip, drop,
                        even, odd, init)


-- 2. Recursion on lists -----------------------------------------------------------
-- help: todo: why error?
{-}product        :: Num a => [a] -> a
product []     = 1
product (n:ns) = n * product ns
-}

reverse        :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]


(++)         :: [a] -> [a] -> [a]
[] ++ ys     = ys
(x:xs) ++ ys = x : (xs ++ ys)


insert                       :: Ord a => a -> [a] -> [a]
insert x []                   = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys


insertionSort          :: Ord a => [a] -> [a]
insertionSort []       = []
insertionSort (x : xs) = insert x (insertionSort xs)


init                   :: [a] -> [a]
init [_]                = []
init (x:xs) | null xs   = []
            | otherwise = x : init xs


-- 3. Multiple arguments ----------------------------------------------------------

zip               :: [a] -> [b] -> [(a, b)]
zip [] _          = []
zip _ []          = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys



drop         :: Int -> [a] -> [a]
drop 0 xs     = xs
drop n []     = []
drop n (_:xs) = drop (n-1) xs





-- 4. Multiple recursion ----------------------------------------------------------
qsort        :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                    smaller = [a | a <- xs, a <= x]
                    larger  = [b | b <- xs, b > x]





-- 5. Mutual Recursion -------------------------------------------------------------
even   :: Int -> Bool
even 0 = True
even n = odd (n-1)

odd   :: Int -> Bool
odd 0 = False
odd n = even (n-1)


-- extracting elemenst from all even and odd positions
evens          :: [a] -> [a]
evens []       = []
evens (x : xs) = x : odds xs

odds           :: [a] -> [a]
odds []        = []
odds (_ : xs) = evens xs


main = do
    --print $ product [1,2,3,4,5]
    print $ reverse [1,2,3,49, 89, 34, -1, 10, 333]
    print ( [1,2,3] ++ [4,5])
    print $ insert 10 [1,3,90, 80]
    putStr "Insert sort: "; print $ insertionSort [-1, 90, 45, 3, 4, 7, 80, 1, 22, 0]
    print $ zip ['a', 'b', 'c'] [1,2,3,4]
    print $ drop 3 [1,2,3,4,5]
    putStr "Qsort: "; print $ qsort [-1, 90, 45, 3, 4, 7, 80, 1, 22, 0]
    print $ even 4; print $ odd 5; print $ even 5; print $ odd 4
    print $ evens "abcde"; print $ odds "abcde"
    print $ init "abcdefghifkl"; print $ init "ab"; print $ init "a"

import Test.QuickCheck


mergeSort :: Ord a => ([a] -> [a] -> [a]) -> [a] -> [a]
mergeSort merge xs
    | length xs < 2 = xs
    | otherwise = merge (mergeSort merge first) (mergeSort merge second)
    where first = take half xs
          second = drop half xs
          half = (length xs) `div` 2

ordering :: Ord a => a -> a -> Ordering
ordering x y
    | x < y = LT
    | x == y = EQ
    | otherwise = GT

-- HELP is this how to do this exercise?
-- note sorting on freqs
freqMerge :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
freqMerge xs [] = xs
freqMerge [] ys = ys
freqMerge ((p,n):xs) ((q,m):ys)
    | ordering n m == LT || (ordering n m == EQ && ordering p q == LT)
            = (p, n) : freqMerge xs ((q,m):ys)
    | otherwise = (q,m) : freqMerge ((p,n):xs) ys





alphaMerge :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
alphaMerge xs [] = xs
alphaMerge [] ys = ys
alphaMerge ((p,n):xs) ((q,m):ys)
    | p == q    = (p, n+m) : alphaMerge xs ys
    | p < q     = (p, n) : alphaMerge xs ((q,m):ys)
    | otherwise = (q, m) : alphaMerge ((p,n):xs) ys


-- precondition - the two lists must be individually sorted.
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys





sortedSnd :: Ord b => [(a,b)] -> Bool
sortedSnd [] = True
sortedSnd [p] = True
sortedSnd [p1, p2] = inOrderSnd p1 p2
sortedSnd (p1:p2:ps)
    | inOrderSnd p1 p2 = True && sortedSnd (p2:ps)
    | otherwise = False

inOrderSnd :: Ord b => (a,b) -> (a,b) -> Bool
inOrderSnd p1 p2 = snd p1 <= (snd p2)


propMergeSortFreq :: [(Char, Int)] -> Bool
propMergeSortFreq ps = sortedSnd (mergeSort freqMerge ps)
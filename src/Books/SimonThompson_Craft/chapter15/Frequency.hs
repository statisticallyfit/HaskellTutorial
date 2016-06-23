import Test.QuickCheck
--module Frequency (frequency) -- [Char] -> [(Char, Int)]


-- NOTE: take "battat" text and order each tuple with increasing frequency order

{-
PROCESS

1. pair each char with count of 1
2. amalgamate counts of equal characters while sorting on the list of characters.
3. sort by increasing freq order.

-- Effect: we have list of tuples sorted first by char and then by freq.
-}




frequency :: [Char] -> [(Char, Int)]
frequency = mergeSort freqMerge . mergeSort alphaMerge . map start -- string arg here
    where start ch = (ch, 1)



----------------------------------------------------------------------------------------------
mergeSort :: Ord a => ([a] -> [a] -> [a]) -> [a] -> [a]
mergeSort merge xs
    | length xs < 2 = xs
    | otherwise = merge (mergeSort merge first) (mergeSort merge second)
    where first = take half xs
          second = drop half xs
          half = (length xs) `div` 2


-- note sorting on chars
-- input: all ints are equal to 1
-- output: all tuples are in sorted char order and ints are amalgamated for identical chars.
alphaMerge :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
alphaMerge xs [] = xs
alphaMerge [] ys = ys
alphaMerge ((p,n):xs) ((q,m):ys)
    | p == q    = (p, n+m) : alphaMerge xs ys
    | p < q     = (p, n) : alphaMerge xs ((q,m):ys)
    | otherwise = (q, m) : alphaMerge ((p,n):xs) ys


-- note sorting on freqs
freqMerge :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
freqMerge xs [] = xs
freqMerge [] ys = ys
freqMerge ((p,n):xs) ((q,m):ys)
    | n < m || (n == m && p < q) = (p, n) : freqMerge xs ((q,m):ys)
    | otherwise = (q,m) : freqMerge ((p,n):xs) ys





-- Testing --------------------------------------------------------------------------------------

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted [x,y] = x <= y
sorted (a:b:cs)
    | a <= b = True && sorted (b:cs)
    | otherwise = False

-- precondition - the two lists must be individually sorted.
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

------------------------------------------------------
mergeFst :: Ord a => [(a,b)] -> [(a,b)] -> [(a,b)]
mergeFst [] ys = ys
mergeFst xs [] = xs
mergeFst (x:xs) (y:ys)
    | fst x < (fst y) = x : mergeFst xs (y:ys)
    | otherwise = y : mergeFst (x:xs) ys

mergeSnd :: Ord b => [(a,b)] -> [(a,b)] -> [(a,b)]
mergeSnd [] ys = ys
mergeSnd xs [] = xs
mergeSnd (x:xs) (y:ys)
    | snd x < (snd y) = x : mergeSnd xs (y:ys)
    | otherwise = y : mergeSnd (x:xs) ys

----------------------------------------------------

sortedFst :: Ord a => [(a,b)] -> Bool
sortedFst [] = True
sortedFst [p] = True
sortedFst [p1, p2] = inOrderFst p1 p2
sortedFst (p1:p2:ps)
    | inOrderFst p1 p2 = True && sortedFst (p2:ps)
    | otherwise = False

sortedSnd :: Ord b => [(a,b)] -> Bool
sortedSnd [] = True
sortedSnd [p] = True
sortedSnd [p1, p2] = inOrderSnd p1 p2
sortedSnd (p1:p2:ps)
    | inOrderSnd p1 p2 = True && sortedSnd (p2:ps)
    | otherwise = False

inOrderFst :: Ord a => (a,b) -> (a,b) -> Bool
inOrderFst p1 p2 = fst p1 <= (fst p2)

inOrderSnd :: Ord b => (a,b) -> (a,b) -> Bool
inOrderSnd p1 p2 = snd p1 <= (snd p2)


-----------------------------------------------------

propMergeSort :: [Int] -> Bool -- if we use type 'a' it will give error saying "ambiguous"
propMergeSort xs = sorted (mergeSort merge xs)

propMergeSortFsts :: [(Char, Int)] -> Bool
propMergeSortFsts ps = sortedFst (mergeSort mergeFst ps)

propMergeSortSnds :: [(Char, Int)] -> Bool
propMergeSortSnds ps = sortedSnd (mergeSort mergeSnd ps)

propMergeSortAlpha :: [(Char, Int)] -> Bool
propMergeSortAlpha ps = sortedFst (mergeSort alphaMerge ps)

propMergeSortFreq :: [(Char, Int)] -> Bool
propMergeSortFreq ps = sortedSnd (mergeSort freqMerge ps)

test1 = quickCheck propMergeSort
test2 = quickCheck propMergeSortFsts
test3 = quickCheck propMergeSortSnds
test4 = quickCheck propMergeSortAlpha
test5 = quickCheck propMergeSortFreq
import Test.QuickCheck

mergeSort :: Ord a => ([a] -> [a] -> [a]) -> [a] -> [a]
mergeSort merge xs
    | length xs < 2 = xs
    | otherwise = merge (mergeSort merge first) (mergeSort merge second)
    where first = take half xs
          second = drop half xs
          half = (length xs) `div` 2


-- note leaves no duplciates .
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | x == y = x : merge xs ys
    | otherwise = y : merge (x:xs) ys

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted [x,y] = x <= y
sorted (a:b:cs)
    | a <= b = True && sorted (b:cs)
    | otherwise = False




propMergeSort :: [Int] -> Bool -- if we use type 'a' it will give error saying "ambiguous"
propMergeSort xs = sorted (mergeSort merge xs)
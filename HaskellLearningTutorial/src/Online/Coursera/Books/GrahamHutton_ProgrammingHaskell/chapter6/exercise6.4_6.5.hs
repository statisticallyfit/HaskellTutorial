

merge           :: Ord a => [a] -> [a] -> [a]
merge [] ys     = ys
merge xs []     = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


halve    :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs



mergeSort      :: Ord a => [a] -> [a]
mergeSort []   = []
mergeSort [e]  = [e]
mergeSort list = merge (mergeSort xs) (mergeSort ys)
                 where (xs, ys) = halve list


main = do
    print $ merge [2,5,6] [1,3,4]
    print $ mergeSort [1, 90, 89, -10, 3, 4, 7, 5, 1, 1, 1, 2, 1, 0]
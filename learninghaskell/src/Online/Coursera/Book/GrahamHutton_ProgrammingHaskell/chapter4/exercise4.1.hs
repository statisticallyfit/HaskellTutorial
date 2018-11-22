
-- Define function halve that splits an even-lengthed
-- list into two halves.
-- assume: xs is even-lengthed
halve :: [a] -> ([a], [a])
halve xs = splitAt ((length xs) `div` 2) xs

halve' :: [a] -> ([a], [a])
halve' xs = (take n xs, drop n xs)
                where n = length xs `div` 2

main = do
    print (halve [1, 2, 3, 4, 5, 6, 7, 8])
    print (halve [1, 2, 3, 4])
    print (halve [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16])
    print (halve' [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14])
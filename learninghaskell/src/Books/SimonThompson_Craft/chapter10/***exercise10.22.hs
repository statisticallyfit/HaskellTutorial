import Test.QuickCheck


split :: [a] -> ([a], [a])
split [] = ([], [])
split [a] = ([a], [])
split (a : b : rest) = (a : left, b : right)
        where (left, right) = split rest

merge :: ([a], [a]) -> [a]
merge ([], []) = []
merge ([a], []) = [a]
merge (a : left, b : right) = a : b : merge (left, right)


propSplitMerge    :: [Integer] -> Bool
propSplitMerge xs = merge (split xs) == xs

-- HELP hwy does this give up?
propMergeSplit :: [Integer] -> [Integer] -> Property
propMergeSplit xs ys = length xs == (length ys + 1)
    ==>
    split (merge (xs, ys)) == (xs, ys)
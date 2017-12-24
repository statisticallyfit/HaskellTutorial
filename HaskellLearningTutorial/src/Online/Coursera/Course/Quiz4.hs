import Data.List


-- Question 7 : implement remove
remove      :: Int -> [a] -> [a]
remove n xs = take n xs ++ drop (n+1) xs



main = do
    print (remove 4 [0,1,2,3,4,5,6,7])
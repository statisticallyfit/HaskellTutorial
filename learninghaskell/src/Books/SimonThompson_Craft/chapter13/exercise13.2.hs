

numEqual :: Eq a => a -> [a] -> Int
numEqual elem xs = length [1 | x <- xs, elem == x]


member :: Eq a => a -> [a] -> Bool
member elem xs = (numEqual elem xs) /= 0




numEqual' :: Eq a => a -> [a] -> Int
numEqual' elem xs = nums elem xs 0
    where nums _ [] acc = acc
          nums elem (x:xs) acc
            | elem == x = nums elem xs (acc + 1)
            | otherwise = nums elem xs acc

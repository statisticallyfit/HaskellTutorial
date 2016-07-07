import Data.List


ex = take 10 $ unfoldr (\b -> Just (b, b+1)) 0


mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
    where go :: Num a => a -> [a] -> a
          go n [] = n
          go n (x:xs) = go (n+x) xs

niceSum :: Num a => [a] -> a
niceSum xs = foldl (+) 0 xs

------------
mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
    where go :: Num a => a -> [a] -> a
          go n [] = n
          go n (x:xs) = go (n*x) xs

niceProduct :: Num a => [a] -> a
niceProduct = foldl (*) 1 -- xs here

---------------
mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
    where go :: [a] -> [[a]] -> [a]
          go xs' [] = xs'
          go xs' (x:xs) = go (xs' ++ x) xs

niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []


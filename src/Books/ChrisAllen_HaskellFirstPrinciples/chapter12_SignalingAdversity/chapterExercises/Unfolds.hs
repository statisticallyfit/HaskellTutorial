import Data.List


ex = take 10 $ unfoldr (\b -> Just (b, b+1)) 0


mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
    where go :: Num a => a -> [a] -> a
          go n [] = n
          go n (x:xs) = go (n+x) xs 
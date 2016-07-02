

factors :: Int -> [Int]
factors n = filter (\x -> mod n x == 0) upTo
    where upTo = [1 .. n]

{-

hamming :: [Int]
hamming = filter (map isHamming) factorsOfEachNum
    where factorsOfEachNum = map factors [1..]
          isEither235 n = n == 2 || n == 3 || n == 5
          isHamming facList = filter isEither235 facList-}

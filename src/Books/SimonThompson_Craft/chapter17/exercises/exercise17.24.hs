

makeFactors :: Int -> [Int]
makeFactors n = filter (\x -> mod n x == 0) upTo
    where upTo = [1 .. n]



hamming :: [Int]
hamming = filter (map isHamming) factorsOfEachNum
    where factorLists = map makeFactors [1..]
          onlyHamPrimes fs = filter isHamPrime fs
          isHamPrime n = n == 1 || n == 2 || n == 3 || n == 5
          hasFactorsOtherThanHamPrimes fs = length fs /= (length onlyHamPrimes fs)
          isHamming factorLists = filter isHamPrime factorLists


-- main code: filter (\n -> isHamming (factors n)) nums
-- isHamming facs = (length $ filter isEither235 facs) /= 0
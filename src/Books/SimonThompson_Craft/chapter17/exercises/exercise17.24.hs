

makeFactors :: Int -> [Int]
makeFactors n = filter (\x -> mod n x == 0) upTo
    where upTo = [1 .. n]

isPrime :: Int -> Bool
isPrime num = (length $ makeFactors num) == 2

highestPrime :: Int -> Int
highestPrime num = head $ reverse primeFactors
    where factors = makeFactors num
          primeFactors = filter isPrime factors


-- checks to see if the number's greatest prime factor is 5,3,2 or 1.
isHamming :: Int -> Bool
isHamming num = (highestPrime num == 5 || highestPrime num == 3 ||
                 highestPrime num == 2 || highestPrime num == 1)

{-

hamming :: [Int]
hamming = filter (map isHamming) factorLists
    where factorLists = map makeFactors [1..]
-}

{-

hamming :: [Int]
hamming = filter (map isHamming) factorLists
    where factorLists = map makeFactors [1..]
          onlyHamPrimes fs = filter isHamPrime fs
          isHamPrime n = n == 1 || n == 2 || n == 3 || n == 5
          hasFactorsOtherThanHamPrimes fs = length fs /= (length onlyHamPrimes fs)
          isHamming factorLists = filter isHamPrime factorLists
-}


-- main code: filter (\n -> isHamming (factors n)) nums
-- isHamming facs = (length $ filter isEither235 facs) /= 0
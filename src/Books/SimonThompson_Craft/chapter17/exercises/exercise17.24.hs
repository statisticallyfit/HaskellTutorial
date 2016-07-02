

makeFactors :: Int -> [Int]
makeFactors n = filter (\x -> mod n x == 0) upTo
    where upTo = [1 .. n]

isPrime :: Int -> Bool
isPrime num = num == 1 || (length $ makeFactors num) == 2

highestPrime :: Int -> Int
highestPrime num = head $ reverse primeFactors
    where factors = makeFactors num
          primeFactors = filter isPrime factors


-- checks to see if the number's greatest prime factor is 5,3,2 or 1.
isHamming :: Int -> Bool
isHamming num = (highestPrime num == 5 || highestPrime num == 3 ||
                 highestPrime num == 2 || highestPrime num == 1)



hamming :: [Int]
hamming = filter isHamming [1.. ]


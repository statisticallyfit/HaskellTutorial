
-- exercise 21 ---------------------------------------------------------------------

-- method 1 - with hel.per methods
split :: [a] -> ([a], [a])
split (a:bs) = ( firstHalf, secondHalf)
    where firstHalf = evenPos a bs
          secondHalf = oddPos a bs

evenPos            :: a -> [a] -> [a]
evenPos a []       = [a]
evenPos a [_]      = [a]
evenPos a (b:c:ds) = a : evenPos c ds

oddPos          :: a -> [a] -> [a]
oddPos a []     = []
oddPos a [b]    = [b]
oddPos a (b:cs) = evenPos b cs
-----------------------------------------------------
-- method 2 -- recursion
split'                :: [a] -> ([a], [a])
split' []             = ([], [])
split' [a]            = ([a], [])
split' (a : b : rest) = (a : left, b : right)
                        where (left, right) = split' rest
-----------------------------------------------------
-- method 3 -- foldr HELP HELP HELP
split''     :: [a] -> ([a], [a])
split'' []  = ([], [])
split'' [a] = ([a], [])
split'' xs  = (foldr firstEvenPos [] xs, foldr firstOddPos [] xs)


firstOddPos          :: a -> [a] -> [a]
firstOddPos a []     = []
firstOddPos a (b:cs) = (b:cs)

firstEvenPos          :: a -> [a] -> [a]
firstEvenPos a []     = [a]
firstEvenPos a (b:cs) = (a:cs)









------------------------------------------------------------------------------------
-- note merge the type:
-- merge ([1,3,5], [2,4]) = [1,2,3,4,5]
-- note will not merge in order if list tuple is unordered.
merge :: ([a], [a]) -> [a]
merge ([], []) = []
merge ([a], []) = [a]
merge (a : left, b : right) = a : b : merge (left, right)
















{-

pickAtEvenPos          :: [a] -> [a]
pickAtEvenPos []       = []
pickAtEvenPos [x]      = [x]
pickAtEvenPos (a:b:cs) = a : pickAtEvenPos cs


pickAtOddPos          :: [a] -> [a]
pickAtOddPos []       = []
pickAtOddPos [x]      = []
pickAtOddPos (a:b:cs) = b : pickAtOddPos cs

pickAtOddPos'          :: [a] -> [a]
pickAtOddPos' []       = []
pickAtOddPos' [x]      = []
pickAtOddPos' (x:xs)   = pickAtEvenPos xs
-}

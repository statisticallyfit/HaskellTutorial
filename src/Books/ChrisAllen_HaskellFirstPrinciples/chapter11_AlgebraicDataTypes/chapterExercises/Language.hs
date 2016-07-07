import Data.List hiding (isSubsequenceOf)
import Data.Maybe
import Test.QuickCheck



-- note tests if each element is greater than the other - doesn't tolerate duplicates.
sortedStrict :: Ord a => [a] -> Bool
sortedStrict xs = xs == sort xs && (length xs == length (nub xs))

--- note tests whether the sub portion is contained in its order inside whole.
-- IT can be spread out so there are other elements from whole interspersed, but it
-- just cannot be in a different order than sub itself.

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf sub whole
    | isSubPresent = isSubSameOrder
    | otherwise = False
    where isSubPresent = and $ map ((flip elem) whole) sub
          justLocs = map ((flip elemIndex) whole) sub
          locs = map fromJust (justLocs)
          isSubSameOrder = sortedStrict locs




--- TESTING ------------------------------------------------------------------------------

--- testing that elements are strictly sorted
testStrictSort :: [Int] -> Bool
testStrictSort xs = (noDups xs && sorted xs) == sortedStrict xs
    where noDups xs = nub xs == xs
          sorted xs = sort xs == xs

--- testing that reverse word is not subsequence of word itself.
testSubReverse :: String -> Bool
testSubReverse word = if (length word == 0 || length word == 1) then True else mainOccs
    where mainOccs = isSubsequenceOf word (reverse word) == False


--- testing if is subsequence then automatically each letter of sub is inside the whole.
testSubContainLetter :: String -> String -> Bool
testSubContainLetter sub whole = (isSubsequenceOf sub whole) == containedLetters
    where containedLetters = and $ map ((flip elem) whole) sub
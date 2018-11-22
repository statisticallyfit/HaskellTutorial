module ValidatingNumbersIntoWords where

import Data.List -- (intersperse)
import qualified Data.Map as M
import Test.Hspec
import Test.QuickCheck



-- note returns 9 -> "nine"
digitToWord :: Int -> String
digitToWord n = case M.lookup n mapped of
                    Just s -> s
                    Nothing -> ""
    where mapped = M.fromList [(0,"zero"), (1,"one"),(2,"two"),(3,"three"),(4,"four"),
                            (5,"five"), (6,"six"),(7,"seven"),(8,"eight"),(9,"nine")]

-- note takes many-digited number, and returns a list of its digits.
digits :: Int -> [Int]
digits number
    | number `div` 10 == 0 =  [number `mod` 10]
    | otherwise = (digits newNum) ++ [nextDigitFromEnd]
    where nextDigitFromEnd = number `mod` 10
          newNum = number `div` 10

-- note takes a many-digited number and returns it in word form
-- interspersed with "-"
wordNumber :: Int -> String
wordNumber n = dashedWordDigits
    where wordDigits = map digitToWord (digits n)
          dashedWordDigits = intercalate "-" wordDigits

-- note write these functions then go back to page 551 and write tests.


-- testing ideas
-- testing digits: length show number == length (digits number)
-- except for when number starts with zero. which won't happen.
testDigits = describe "digits converts a number into a list with its digits" $ do
    it "returns [0] for 0" $ do
        digits 0 `shouldBe` [0]
    it "returns [1,2,3] for 123" $ do
        digits 123 `shouldBe` [1,2,3]
    it "ignores zeroes at the front" $ do
        digits 0123 `shouldBe` [1,2,3]
    -- help help todo why does this take forever to compute?
{-
    it "length of digits list always equals number of digits" $ do
        property $ \n -> (length $ digits (n::Int)) == (length $ show (n::Int))

-}


testDigitToWord = describe "digitToWord converts a digit (0-9) into word form" $ do
    it "returns zero for 0" $ do
        digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
        digitToWord 1 `shouldBe` "one"
    it "returns two for 2" $ do
        digitToWord 2 `shouldBe` "two"
    it "returns three for 3" $ do
        digitToWord 3 `shouldBe` "three"
    it "returns four for 4" $ do
        digitToWord 4 `shouldBe` "four"
    it "returns five for 5" $ do
        digitToWord 5 `shouldBe` "five"
    it "returns six for 6" $ do
        digitToWord 6 `shouldBe` "six"
    it "returns seven for 7" $ do
        digitToWord 7 `shouldBe` "seven"
    it "returns eight for 8" $ do
        digitToWord 8 `shouldBe` "eight"
    it "returns nine for 9" $ do
        digitToWord 9 `shouldBe` "nine"


testWordNumber = describe "wordNumber converts digit into word-dashed form" $ do
    it "returns one for 1" $ do
        wordNumber 1 `shouldBe` "one"
    it "returns one-zero-zero for 100" $ do
        wordNumber 100 `shouldBe` "one-zero-zero"
    it "returns nine-zero-zero-one for 9001" $ do
        wordNumber 9001 `shouldBe` "nine-zero-zero-one"




main :: IO()
main = hspec $ do
    testDigits -- help fix
    testDigitToWord
    testWordNumber
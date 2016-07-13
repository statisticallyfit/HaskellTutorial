module TestVigniereCipher where


import VigniereCipher

import Data.Char
import Data.List

import Test.QuickCheck
import Test.Hspec



testRemoveSpaces :: SpecWith()
testRemoveSpaces = describe "removeSpaces" $ do
    it "spaces should be gone, but newline and tab can stay" $ property $
        \txt -> (elem ' ' (removeSpaces (txt :: String))) == False


testIdentityPutGetSpaces :: SpecWith ()
testIdentityPutGetSpaces = describe "putSpaces and getSpaces" $ do
    it "putSpaces and getSpaces are inverses" $ property $
        \txt -> (putSpaces (getSpaces txt) (removeSpaces txt)) == (txt :: String)


testNoShift :: SpecWith()
testNoShift = describe "no shift" $ do
    it "shifting with same character results in no shift" $ property $
        \char -> getShift ((char :: Char), (char :: Char)) == 0

testOneShift :: SpecWith()
testOneShift = describe "one shift" $ do
    it "shifting once means a difference of one letter,comma,num char..." $ property $
        \c1 -> getShift ((c1 :: Char), chr (ord c1 + 1)) == 1

testShiftEquality :: SpecWith()
testShiftEquality = describe "shift equality" $ do
    it "shifting one way is the same as shifting the other way" $ property $
        \c1 c2 -> getShift ((c1 :: Char), (c2 :: Char)) == getShift (c2, c1)

testReplicateKeyWord :: SpecWith()
testReplicateKeyWord = describe "replicateKeyWord" $ do
    it "replicated keyword length == text length and has same spaces" $ do
    (replicateKeyWord "Meet at dawn" "ALLY") `shouldBe` "ALLY AL LYAL"


testWrapAround :: SpecWith()
testWrapAround = describe "wraparound aspect" $ do
    it "when letter in text is close to 'z' and keyword shift pushes it beyond 'z', \
        \ then the result code is pushed back to go from 'a' again" $ do
        (encodeVigniere "MEET AT DAWNW" "ally") `shouldBe` "MPPR AE OYWYH"


testEncodeVigniere :: SpecWith()
testEncodeVigniere = describe "encode vigniere" $ do
    it "length and case in final code should match that of text" $ property $
        \txt key -> checkSameLenCase txt (encodeVigniere (txt :: String) (key :: String))


------------------------------------------------------------------------------------------
-- note he.lper functions
caseMatch (c,t) = (isUpper c && isUpper t) || (isLower c && isLower t)
sameLen text code = length code == length text
sameCase pairs = and $ map caseMatch pairs
checkSameLenCase text code = (sameLen text code) && (sameCase (zip text code))
------------------------------------------------------------------------------------------


main :: IO()
main = hspec $ do
    testRemoveSpaces
    testIdentityPutGetSpaces
    testNoShift
    testOneShift
    testShiftEquality
    testReplicateKeyWord
    testWrapAround
    --testEncodeVigniere -- help too slow how to make it faster?
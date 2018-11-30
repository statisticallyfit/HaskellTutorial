module CaesarCipherSpec (main, spec) where

import Data.Char
import CaesarCipher

import Test.QuickCheck
import Test.Hspec



main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    testIdentityCode
    testLowToInt
    testUppToInt
    testIntToLow
    testIntToUpp


testIdentityCode :: SpecWith()
testIdentityCode
    = describe "Encoding then decoding should equal the input" $ do

    it "decode . encode == input" $ property $
        \fctr txt -> if goodInputText (txt :: String)
                     then (decode (fctr :: Int) $
                          encode fctr txt) == txt
                     else True -- shuttle along, ignoring bad input


testLowToInt :: SpecWith()
testLowToInt = describe "converts lowercase alphabet into number from 0..25" $ do
    it "from 'a'..'z' --->  0 .. 25" $ property $
        \letter -> if goodInputLetter (letter :: Char)
                   then (inRange 0 25 $ lowToInt (toLower letter))
                   else True -- shuttle test along


testUppToInt :: SpecWith()
testUppToInt = describe "converts uppercase alphabet into number from 0 to 25" $ do
    it "from 'A'..'Z' --->  0 .. 25" $ property $
        \letter -> if goodInputLetter (letter :: Char)
                   then (inRange 0 25 $ uppToInt (toUpper letter))
                   else True -- shuttle test along

testIntToLow :: SpecWith()
testIntToLow = describe "converts int from 0 .. 25 into lower case alphabet letter" $ do
    it "from 0..25 --->  'a'..'z'" $ property $
        \num -> if inRange 0 25 (num :: Int)
                then (inRange 'a' 'z' $ intToLow num)
                else True --shuttle along

testIntToUpp :: SpecWith()
testIntToUpp = describe "converts int from 0 .. 25 into uppercase alphabet letter" $ do
    it "from 0..25 --->  'A'..'Z'" $ property $
        \num -> if inRange 0 25 (num :: Int)
                then (inRange 'A' 'Z' $ intToUpp num)
                else True --shuttle along

------------------------------------------------------------------------------------------
-- he.lper functions
goodInputText text = (length text /= 0) && (and $ map isAscii text)
goodInputLetter lt = elem (toLower lt) ['a'..'z']
inRange low high x = x >= low && x <= high

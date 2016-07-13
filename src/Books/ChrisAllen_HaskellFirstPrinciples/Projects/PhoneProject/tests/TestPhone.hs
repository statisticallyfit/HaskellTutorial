module TestPhone where

import Phone

import Data.Char
import Data.List
import Data.Maybe
import Test.Hspec
import Test.QuickCheck


{-

testCountKey :: SpecWith ()
testCountKey = describe "countKey"-}


testIdentityTokenButton :: SpecWith()
testIdentityTokenButton
    = describe "tokenButtonize and buttonTokenize should be inverses of each other" $ do

    {-it "can tell difference from capitals and lowercase" $ do
    (buttonTokenize $ tokenButtonize "HI 123 tHEre") == "HI 123 tHEre"

    it "can tell difference from english and numbers" $ do
    (buttonTokenize $ tokenButtonize "+ #,.?!123abc123..?.a") == "+ #,.?!123abc123..?.a"
-}
        context "when used with [Token]" $ do
            it "always tells the difference from english and numbers" $ property $
           \tokens -> (buttonTokenize . tokenButtonize) tokens == (tokens :: String)




runTests = hspec $ do
    testIdentityTokenButton
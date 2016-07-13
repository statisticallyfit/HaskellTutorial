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


-- note need to tap the stuff under "it" two spaces to the right or else error!
testIdentityTokenButton :: SpecWith()
testIdentityTokenButton
    = describe "tokenButtonize and buttonTokenize should be inverses of each other" $ do

    it "can tell difference from capitals and lowercase" $ do
      (buttonTokenize $ tokenButtonize "HI 123 tHEre") == "HI 123 tHEre"

    it "can tell difference from english and numbers" $ do
      (buttonTokenize $ tokenButtonize "+ #,.?!123abc123..?.a") == "+ #,.?!123abc123..?.a"

    it "token -> button -> token should be true" $ property $
      \tokens -> if doesOccur tokens tokensAllowed
                 then (buttonTokenize . tokenButtonize) tokens == (tokens :: [Token])
                 else True --- note  just to shuttle the test along

    it "button -> token -> button should be true" $ property $
      \buttons -> if doesOccur buttons buttonsAllowed
                  then (tokenButtonize . buttonTokenize) buttons == (buttons :: [Button])
                  else True --- shuttle along




--- testing helper functions / variables

instance Arbitrary Button where
    arbitrary = elements (buttonsAllowed)

doesOccur items bucket = and $ map ((flip elem) items) bucket
tokensAllowed = (concatMap show [0..9]) ++ ['a'..'z'] ++ ['A'..'Z'] ++ "+#.,?!"
buttonsAllowed = concat [numbers, lowLetters, uppLetters, signs, space]
    where numbers = map (Number $) (concatMap show [0..9])
          lowLetters = map (Letter $) ['a' .. 'z']
          uppLetters = map (CapitalLetter $) ['a' .. 'z']
          signs = map (Sign $) "+.,?!#"
          space = [Spacebar]






runTests = hspec $ do
    testIdentityTokenButton



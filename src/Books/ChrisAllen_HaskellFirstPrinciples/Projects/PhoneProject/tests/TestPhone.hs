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
      \toks -> if doesOccur toks toksAllowed
               then (buttonTokenize . tokenButtonize) toks == (toks :: [Token])
               else True --- note  just to shuttle the test along


testIdentityTokenFinger :: SpecWith()
testIdentityTokenFinger
    = describe "fingerTokenize and tokenFingerize should be inverses of each other" $ do

    it "finger -> token -> finger should be true" $ property $
      \fngs -> if doesOccur fngs fngsAllowed
               then (tokenFingerize . fingerTokenize) fngs == (fngs :: [FingerMove])
               else True


testIdentityFingerButton :: SpecWith ()
testIdentityFingerButton
    = describe "fingerButtonize and buttonFingerize should be inverses of each other" $ do

    it "finger -> button -> finger should be true" $ property $
      \fngs -> if doesOccur fngs fngsAllowed
               then (buttonFingerize . fingerButtonize) fngs == (fngs :: [FingerMove])
               else True



{-
testIdentityEncryptDecrypt :: SpecWith()
testIdentityEncryptDecrypt
    = describe "encrypt and decrypt should be inverses of each other" $ do

    it "encrypt -> decrypt should be true" $ property $
      \tokList -> if preconditionMet tokList [toksAllowed]-}


--- testing helper functions / variables

instance Arbitrary Button where
    arbitrary = elements (btnsAllowed)

doesOccur items bucket = and $ map ((flip elem) bucket) items
toksAllowed = (concatMap show [0..9]) ++ ['a'..'z'] ++ ['A'..'Z'] ++ "+#.,?!"
fngsAllowed = nub $ tokenFingerize toksAllowed
btnsAllowed = tokenButtonize toksAllowed
isPad c = c == EngPad || c == NumPad





runTests = hspec $ do
    testIdentityTokenButton
    testIdentityTokenFinger
    testIdentityFingerButton



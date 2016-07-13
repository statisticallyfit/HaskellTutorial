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
      \toks -> if preconditionMet toks tokensAllowed
               then (buttonTokenize . tokenButtonize) toks == (toks :: [Token])
               else True --- note  just to shuttle the test along

    it "button -> token -> button should be true" $ property $
      \btns -> if preconditionMet btns buttonsAllowed
               then (tokenButtonize . buttonTokenize) btns == (btns :: [Button])
               else True --- shuttle along



testIdentityTokenFinger :: SpecWith()
testIdentityTokenFinger
    = describe "fingerTokenize and tokenFingerize should be inverses of each other" $ do

    it "token -> finger -> token should be true" $ property $
      \toks -> if preconditionMet toks tokensAllowed
               then (fingerTokenize . tokenFingerize) toks == (toks :: [Token])
               else True -- shuttle along

    it "finger -> token -> finger should be true" $ property $
      \fings -> if preconditionMet fings fingersAllowed
                then (tokenFingerize . fingerTokenize) fings == (fings :: [FingerMove])
                else True

testIdentityFingerButton :: SpecWith ()
testIdentityFingerButton
    = describe "fingerButtonize and buttonFingerize should be inverses of each other" $ do

    it "finger -> button -> finger should be true" $ property $
      \fings -> if preconditionMet fings fingersAllowed
                then (buttonFingerize . fingerButtonize) fings == (fings :: [FingerMove])
                else True

    it "button -> finger -> button should be true" $ property $
      \btns -> if preconditionMet btns buttonsAllowed
               then (fingerButtonize . buttonFingerize) btns == (btns :: [Button])
               else True




--- testing helper functions / variables

instance Arbitrary Button where
    arbitrary = elements (buttonsAllowed)

preconditionMet items bucket = and $ map ((flip elem) items) bucket
tokensAllowed = (concatMap show [0..9]) ++ ['a'..'z'] ++ ['A'..'Z'] ++ "+#.,?!"
fingersAllowed = nub $ tokenFingerize tokensAllowed
buttonsAllowed = tokenButtonize tokensAllowed






runTests = hspec $ do
    testIdentityTokenButton
    testIdentityTokenFinger
    testIdentityFingerButton



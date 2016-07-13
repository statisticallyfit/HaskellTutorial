module TestPhone where

import Phone

import Data.Char
import Data.List
import Data.Maybe
import Test.Hspec
import Test.QuickCheck




-- IMPORTANT note button -> token -> button is not always true because of EngPad and NumPad
-- Same is with all 3 below tests - there are oddities needed for calculation that
-- cannot and should not be removed to maintain logicality of program.


-- note need to tap the stuff under "it" two spaces to the right or else error!
testIdentityTokenButton :: SpecWith()
testIdentityTokenButton
    = describe "tokenButtonize and buttonTokenize should be inverses" $ do

    it "can tell difference from capitals and lowercase" $ do
      (buttonTokenize $ tokenButtonize "HI 123 tHEre") == "HI 123 tHEre"

    it "can tell difference from english and numbers" $ do
      (buttonTokenize $ tokenButtonize "+ #,.?!123abc123..?.a") == "+ #,.?!123abc123..?.a"

    it "token -> button -> token should be true" $ property $
      \toks -> if doesOccur toks toksAllowed
               then (buttonTokenize . tokenButtonize) toks == (toks :: [Token])
               else True --- note  just to shuttle the test along


-- note important key: we say length > 2 for both since we cannot distinguish if
-- ('*',2), ('2',1) should be "a" or "2" so we need more finger moves after
-- first ('*',2) fingermove to get more context.
-- note also cases like "aaa" -> "222" when going through case 2 here so we must
-- avoid this situation. 
testIdentityTokenFinger :: SpecWith()
testIdentityTokenFinger
    = describe "fingerTokenize and tokenFingerize should be inverses" $ do

    it "finger -> token -> finger should be true" $ property $
      \fngs -> if doesOccur fngs fngsAllowed
                  && rightLength fngs
               then (tokenFingerize . fingerTokenize) fngs == (fngs :: [FingerMove])
               else True

    it "token -> finger -> token should be true" $ property $
      \toks -> if doesOccur toks toksAllowed
                  && rightLength toks
               then (fingerTokenize . tokenFingerize) toks == (toks :: [Token])
               else True
    where rightLength s = length s > 2


testIdentityFingerButton :: SpecWith ()
testIdentityFingerButton
    = describe "fingerButtonize and buttonFingerize should be inverses" $ do

    it "finger -> button -> finger should be true" $ property $
      \fngs -> if doesOccur fngs fngsAllowed
                  && rightLength fngs
               then (buttonFingerize . fingerButtonize) fngs == (fngs :: [FingerMove])
               else True
    where rightLength s = length s > 2




testIdentityEncryptDecrypt :: SpecWith()
testIdentityEncryptDecrypt
    = describe "encrypt and decrypt should be inverses" $ do

    it "decrypt -> encrypt should be true" $ property $
      \fngList -> if doesOccur (concat fngList) fngsAllowed
                     && rightLengths fngList
                  then (encrypt . decrypt) fngList == (fngList :: [[FingerMove]])
                  else True
    where rightLengths fngList = (filter ((> 2) . length) fngList) == fngList






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
    testIdentityEncryptDecrypt


main = runTests
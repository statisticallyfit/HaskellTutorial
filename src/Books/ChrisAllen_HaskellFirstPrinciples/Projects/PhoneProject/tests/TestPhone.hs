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
        (buttonTokenize $ tokenButtonize "+ #,.?!13ac123..?.a") == "+ #,.?!13ac123..?.a"

    it "token -> button -> token should be true" $ property $
        \toks -> if doesOccur toks toksAllowed
                 then (buttonTokenize . tokenButtonize) toks == (toks :: [Token])
                 else True --- note  just to shuttle the test along

    it "button -> token -> button should be true" $ property $
        \btns -> if doesOccur btns btnsAllowed
                 then (tokenButtonize . buttonTokenize) btns == (btns :: [Button])
                 else True --- note  just to shuttle the test along




testIdentityTokenFinger :: SpecWith()
testIdentityTokenFinger
    = describe "fingerTokenize and tokenFingerize should be inverses" $ do

    it "finger -> token -> finger should be true" $ property $
        \fngs -> if doesOccur fngs fngsAllowed
                    && isNotCapitalize fngs
                 then (tokenFingerize . fingerTokenize) fngs == (fngs :: [FingerMove])
                 else True

    it "token -> finger -> token should be true" $ property $
        \toks -> if doesOccur toks toksAllowed
                 then (fingerTokenize . tokenFingerize) toks == (toks :: [Token])
                 else True
    where isNotCapitalize fngs = if fngs == [] then True else head fngs /= ('*',1)


testIdentityFingerButton :: SpecWith ()
testIdentityFingerButton
    = describe "fingerButtonize and buttonFingerize should be inverses" $ do

    it "finger -> button -> finger should be true" $ property $
        \fngs -> if doesOccur fngs fngsAllowed
                    && isNotCapitalize fngs
                 then (buttonFingerize . fingerButtonize) fngs == (fngs :: [FingerMove])
                 else True

    it "button -> finger -> button should be true" $ property $
         \btns -> if doesOccur btns btnsAllowed
                  then (fingerButtonize . buttonFingerize) btns == (btns :: [Button])
                  else True --- note  just to shuttle the test along

    where isNotCapitalize fngs = if fngs == [] then True else head fngs /= ('*',1)



testIdentityEncryptDecrypt :: SpecWith()
testIdentityEncryptDecrypt
    = describe "encrypt and decrypt should be inverses" $ do

    it "decrypt -> encrypt should be true" $ property $
        \fngList -> if doesOccur (concat fngList) fngsAllowed
                    then (encrypt . decrypt) fngList == (fngList :: [[FingerMove]])
                    else True



testMostPopularLetter :: SpecWith()
testMostPopularLetter
    = describe "returns most popular letter" $ do

    it "returns first letter that is most popular, despite ties" $ do
        'h' == (mostPopularLetter "hi there how are you today?")
            `shouldBe` True

    it "returns most popular letter despite its position in the string" $ do
        'a' == (mostPopularLetter "ooppaaaaa lll") `shouldBe` True


testPresses :: SpecWith()
testPresses
    = describe "presses does not account for capitals" $ do

    it "returns same amount for lowercase and uppercase" $ do
        presses 'k' == presses 'K' `shouldBe` True

    it "presses of a number is always greater than or equal to presses of a letter \
        \ because numbers come after letters in keypad" $ do
        presses '2' >= presses 's' `shouldBe` True


testCost :: SpecWith ()
testCost
    = describe "cost takes into account capitals" $ do

    it "returns one greater than press if given capital" $ do
        presses 'k' + 1 == cost 'K' `shouldBe` True

    it "returns same amount if given lowercase letter" $ property $
        \token -> if isLower (token :: Token) && isAscii (token :: Token)
                  then presses (token :: Token) == cost (token :: Token)
                  else True -- shuttle along, ignore non-lowercase letters.


testIdentityRavelUnravel :: SpecWith()
testIdentityRavelUnravel
    = describe "ravel must be inverse of unravel and vice versa" $ do

    it "ravel -> unravel must be true only if token is lowercase" $ property $
        \tok -> if doesOccur [tok] toksAllowed
                   && checkIfLower tok
                then (unravel . ravel) tok == (tok :: Token)
                else True -- shuttle along, ignore non-allowed tokens like @ or %.

    it "unravel -> ravel must be true" $ property $ do
        \fng -> if doesOccur [fng] fngsAllowed
                   && isNotCapitalize fng
                then (ravel . unravel) fng == (fng :: FingerMove)
                else True -- shuttle along

    where checkIfLower t = if isLetter t then isLower t else True
          isNotCapitalize fng = fng /= ('*',1)





--- testing helper functions / variables ------------------------------------------------

instance Arbitrary Button where
    arbitrary = elements (btnsAllowed)

doesOccur items bucket = and $ map ((flip elem) bucket) items
toksAllowed = (concatMap show [0..9]) ++ ['a'..'z'] ++ ['A'..'Z'] ++ "+#.,?!"
fngsAllowed = nub $ tokenFingerize toksAllowed
btnsAllowed = tokenButtonize toksAllowed
isPad c = c == EngPad || c == NumPad





runTests = hspec $ do
    testIdentityRavelUnravel
    testIdentityTokenButton
    testIdentityTokenFinger
    testIdentityFingerButton
    testIdentityEncryptDecrypt
    testMostPopularLetter
    testPresses
    testCost


main :: IO()
main = runTests




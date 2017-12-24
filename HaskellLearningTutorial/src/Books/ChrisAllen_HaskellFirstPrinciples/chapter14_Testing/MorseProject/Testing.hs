module Testing where

import qualified Data.Map as M
import Morse
import Test.QuickCheck


{-
key to run this:

*Main>
:l src/Books/ChrisAllen_HaskellFirstPrinciples/chapter14_Testing/MorseCode/Testing.hs
src/Books/ChrisAllen_HaskellFirstPrinciples/chapter14_Testing/MorseCode/Morse.hs
src/Books/ChrisAllen_HaskellFirstPrinciples/chapter14_Testing/MorseCode/Main.hs
-}


allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

genChar :: Gen Char
genChar = elements allowedChars

genMorse :: Gen Morse
genMorse = elements allowedMorse


--- TESTING ------------------------------------------------------------------------
testThereAndBackAgain :: Property
testThereAndBackAgain = forAll genChar
    (\c -> ((charToMorse c) >>= morseToChar) == Just c)



------------------------------------------------------------------------------------


main :: IO ()
main = quickCheck testThereAndBackAgain
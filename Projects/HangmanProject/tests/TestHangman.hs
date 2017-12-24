module Tests where

import Hangman
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Maybe
import Data.Char
import Data.List
import Control.Monad


--- TESTING ------------------------------------------------------------------------

testFillCharsAlreadyGuessed :: SpecWith ()
testFillCharsAlreadyGuessed
    = describe "fillInCharacter already guessed case" $ do

    it "if 'a' in rabbit was already guessed, then new guess list \
        \ contains two 'a's, one at the front and the other wherever \
        \ it was guessed" $ do
        newGuesses `shouldBe` "axyruiopa"

    it "if 'a' in rabbit was already guessed, then new discovered list \
        \ should remain the same" $ do
        newMaybeDiscs `shouldBe` maybeDiscs

    it "in any condition, discovered list should be \
        \ present in the new and old guessed lists" $ do
        (currGuesses `contains` discs) `shouldBe` True
        (newGuesses `contains` discs) `shouldBe` True

    it "in any case, discovered list should contain no \
        \repeated elements" $ do
        (length $ nub discs) == length discs `shouldBe` True
        (length $ nub newDiscs) == length newDiscs `shouldBe` True

    where guessLetter = 'a'
          word = "rabbit"
          maybeDiscs = [Just 'r',Just 'a',Nothing,Nothing,Just 'i',Nothing]
          discs = getDiscs maybeDiscs
          newDiscs = getDiscs newMaybeDiscs
          currGuesses = "xyruiopa"
          updatedPuzzle
            = fillInCharacter (Puzzle word maybeDiscs currGuesses) guessLetter
          (Puzzle _ newMaybeDiscs newGuesses) = updatedPuzzle
          -- local function to test containment
          contains bucket items = and $ map ((flip elem) bucket) items
          getDiscs ds = filter (not . isSpace) $ fmap (fromMaybe ' ') ds


testFillCharsCorrectlyGuessed :: SpecWith ()
testFillCharsCorrectlyGuessed
    = describe "fillInCharacter correct case" $ do

    it "if 'a' in rabbit was correctly guessed, then discovered list should \
        \ contain no more than one 'a' in the order of the word rabbit" $ do
        newDiscs `shouldBe` "rai"

    it "if 'a' in rabbit was correctly guessed, then guessed list should \
        \ also contain no more than one 'a' at its front" $ do
        newGuesses `shouldBe` (['a'] ++ currGuesses)

    it "in any condition, discovered list should be present in the new \
        \ and old guessed lists" $ do
        (currGuesses `contains` discs) `shouldBe` True
        (newGuesses `contains` discs) `shouldBe` True

    it "in any case, discovered list should contain no \
        \repeated elements" $ do
        (length $ nub discs) == length discs `shouldBe` True
        (length $ nub newDiscs) == length newDiscs `shouldBe` True

    where guessLetter = 'a'
          word = "rabbit"
          maybeDiscs = [Just 'r',Nothing,Nothing,Nothing,Just 'i',Nothing]
          discs = getDiscs maybeDiscs
          newDiscs = getDiscs newMaybeDiscs
          currGuesses = "xyruiop"
          updatedPuzzle
            = fillInCharacter (Puzzle word maybeDiscs currGuesses) guessLetter
          (Puzzle _ newMaybeDiscs newGuesses) = updatedPuzzle
          -- local function to test containment
          contains bucket items = and $ map ((flip elem) bucket) items
          getDiscs ds = filter (not . isSpace) $ fmap (fromMaybe ' ') ds


testFillCharsWronglyGuessed :: SpecWith ()
testFillCharsWronglyGuessed
    = describe "fillInCharacter wrong case" $ do

    it "if 'x' was guessed for rabbit, then discovered list should \
        \ remain the same" $ do
        newDiscs `shouldBe` discs

    it "if 'x' was guessed for rabbit, then guessed list should \
        \ contain an 'x' at its front, even if 'x' already is present" $ do
        newGuesses `shouldBe` (['x'] ++ currGuesses)

    it "in any condition, discovered list should be present in the new \
        \ and old guessed lists" $ do
        (currGuesses `contains` discs) `shouldBe` True
        (newGuesses `contains` discs) `shouldBe` True

    it "in any case, discovered list should contain no \
        \repeated elements" $ do
        (length $ nub discs) == length discs `shouldBe` True
        (length $ nub newDiscs) == length newDiscs `shouldBe` True

    where guessLetter = 'x'
          word = "rabbit"
          maybeDiscs = [Just 'r',Nothing,Nothing,Nothing,Just 'i',Nothing]
          discs = getDiscs maybeDiscs
          newDiscs = getDiscs newMaybeDiscs
          currGuesses = "xyruiop"
          updatedPuzzle
            = fillInCharacter (Puzzle word maybeDiscs currGuesses) guessLetter
          (Puzzle _ newMaybeDiscs newGuesses) = updatedPuzzle
          -- local function to test containment
          contains bucket items = and $ map ((flip elem) bucket) items
          getDiscs ds = filter (not . isSpace) $ fmap (fromMaybe ' ') ds


------------------------------------------------------------------------------------

-- note three functions below from:
-- https://github.com/nlander/hangman/blob/master/test/Spec.hs
-- TODO TODO TODO: understand how this code works!
-- TODO TODO TODO: Especially all the monadic functions.


letters :: Gen Char
letters = elements ['a'..'z']

puz :: IO ()
puz = sample $ makePuzzle "chronology"

makePuzzle :: String -> Gen Puzzle
makePuzzle word = do
  guessedRight <- replicateM (length word) arbitrary :: Gen [Bool]
  let
    spotGuess gsdRight wordChar = if gsdRight then Just wordChar else Nothing
    discoveredMaybe = zipWith spotGuess guessedRight word
    guesses = catMaybes discoveredMaybe in
    return (Puzzle word discoveredMaybe guesses)



propFillInCharacters :: Puzzle -> Property
propFillInCharacters puzzle@(Puzzle word _ oldGuesses) =
  forAll letters -- note type forAll :: Gen a -> (a -> prop) -> Property
  (\guessLetter -> if guessLetter`elem` word
             then case fillInCharacter puzzle guessLetter of
                    (Puzzle _ newDiscoveredChars newGuesses) ->
                      Just guessLetter `elem` newDiscoveredChars &&
                      newGuesses == guessLetter : oldGuesses
             else case fillInCharacter puzzle guessLetter of
                    (Puzzle _ newDiscoveredChars newGuesses) ->
                      not (Just guessLetter `elem` newDiscoveredChars) &&
                      newGuesses == guessLetter : oldGuesses)


propHandleGuess :: String -> Property
propHandleGuess word =
  forAll (makePuzzle word)
   (\puzzle@(Puzzle _ _ oldGuesses) -> monadicIO $ forAllM letters
   -- note type of forAllM :: Gen a -> (a -> PropertyM m b) -> PropertyM m b
                (\guessLetter -> do
                  newPuzzle@(Puzzle _ _ newGuesses) <- run $
                                     handleGuess puzzle guessLetter
                  case alreadyGuessed puzzle guessLetter of
                    True  -> return $ puzzle == newPuzzle
                    False -> return $ newGuesses == guessLetter : oldGuesses))




runProperties = do
    word <- randomWord'
    putStr "(1) propFillInCharacters\n(1) "
    quickCheck $ forAll (makePuzzle word) propFillInCharacters
    putStr "(2) propHandleGuess\n(2) "
    quickCheck $ propHandleGuess
    putStrLn ""


runTesting = hspec $ do
    testFillCharsAlreadyGuessed
    testFillCharsCorrectlyGuessed
    testFillCharsWronglyGuessed

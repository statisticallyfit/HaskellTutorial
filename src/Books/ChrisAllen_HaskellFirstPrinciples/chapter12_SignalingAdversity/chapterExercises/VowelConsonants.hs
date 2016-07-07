import Data.List
import Data.Maybe
import Prelude hiding (Word )


------------------------------------------------------------------------------------------
--- 3

countVowels :: String -> Integer
countVowels = fromIntegral . length . (filter ((flip elem) "aeiou"))


------------------------------------------------------------------------------------------
--- 4

newtype Word = Word String deriving (Eq, Show)

vowels = "aeiou"

countConsonants :: String -> Integer
countConsonants = fromIntegral . length . (filter (not . ((flip elem) "aeiou")))


-- note if num vowels exceed num consonants then return nothing, else just the word.
validateWord :: String -> Maybe Word
validateWord word
    | vs > cs = Nothing
    | otherwise = Just (Word word)
    where cs = countConsonants word
          vs = countVowels word



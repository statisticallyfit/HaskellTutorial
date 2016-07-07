import Data.List
import Data.Maybe
import Prelude hiding (Word )


--- 1

-- example >>> "blahtheblah" ==> Just "blahtheblah"
-- example >>> "the" ==> Nothing
-- precondition: just one piece of text no spaces, even if it has 'the' inside it.
notThe :: String -> Maybe String
notThe text
    | text == "the" = Nothing
    | otherwise = Just text


-- precondition: piece of text with spaces
-- postcondition: the parts with "the" are replaced with "a". Does not replace if no
-- spaces occur between "the"s.
replaceThe :: String -> String
replaceThe text = spacify (replaceWithA alerts)
    where alerts = map notThe (words text)
          replaceWithA as = map (\jw -> if jw == Nothing then "a" else fromJust jw) as
          spacify ws = intercalate " " ws


------------------------------------------------------------------------------------------
--- 2

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel text = count (words text) 0
    where count [] acc = acc
          count [w] acc = acc
          count (w:ws) acc
            | w == "the" && (isVowel nextLetter) = count (tail ws) (acc + 1)
            | otherwise = count ws acc
            where nextLetter = head $ head ws
                  isVowel x = elem x "aeiou"


countTheBeforeVowel' :: String -> Int
countTheBeforeVowel' text = length (filter isVowel wordsAfterThe)
    where wordsAfterThe = getWordsAfterThe (words text) []
          isVowel w = elem (head w) "aeiou"
          getWordsAfterThe [] accs = accs
          getWordsAfterThe [_] accs = accs
          getWordsAfterThe (w:ws) accs
            | w == "the" = getWordsAfterThe (tail ws) (accs ++ [head ws])
            | otherwise = getWordsAfterThe ws accs



--- TESTING ------------------------------------------------------------------------------

testReplaceThe :: String -> Bool
testReplaceThe text = (not $ isSubsequenceOf "the" (replaceThe text))

import Data.List
import Data.Char (toUpper, isUpper, isAlpha, isSpace, isAscii)
import Data.Function (on)
import Test.QuickCheck



-- precondition expecting just one word with no spaces!
-- If given spaces, takes first word only.
capitalizeWord' :: String -> String
capitalizeWord' possibleWord = cap word
    where word = head $ words possibleWord
          cap w = [toUpper $ head w] ++ tail w


capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs)
    | isAlpha x = toUpper x : xs
    | otherwise = x : capitalizeWord xs


-- note if last sentence doesn't end in period, this function ignores it.
-- note key: takes sentences marked by periods and capitalizes their first letter.
-- had major help here. Understand more how the (==) `on` (== '.') part words.
capitalizeParagraph :: String -> String
capitalizeParagraph paragraph
    = concatMap capitalizeWord $ groupBy ((==) `on` (=='.')) paragraph







--- TESTING ------------------------------------------------------------------------------


--- testing that result first letter is indeed upper
-- note ignore words that are not ascii like "\223" or words that are made of space.
-- Just shuttle out a True to keep the tests working and let real tests work.
testCapWord :: String -> Bool
testCapWord word
    | length word == 0 = True
    | otherwise = toUpper firstLetter == (head $ capitalizeWord word)
    where noFrontSpace = dropWhile (== ' ') word
          firstLetter = head word


testCapParagraphLen :: String -> Bool
testCapParagraphLen par = length par == (length $ capitalizeParagraph par)



main = do
    quickCheck testCapWord
    quickCheck testCapParagraphLen
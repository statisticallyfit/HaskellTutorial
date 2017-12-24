type Text = String


whitespace = ['\n', '\t', ' ']

getUntil :: (a -> Bool) -> [a] -> [a]
gteUntil p []   = []
getUntil p (x:xs)
    | p x       = []
    | otherwise = x : getUntil p xs


dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil p [] = []
dropUntil p (x:xs)
    | p x = (x:xs)
    | otherwise = dropUntil p xs

--------------------------------------------

dropSpace :: Text -> Text
dropSpace xs = dropUntil isNotWhitespace xs

dropWord :: Text -> Text
dropWord xs = dropUntil isWhitespace xs

getWord :: Text -> Text
getWord xs = getUntil isWhitespace xs

-----------------------------------------------------------------------------------


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p xs = getUntil notP xs
    where notP x = not (p x)
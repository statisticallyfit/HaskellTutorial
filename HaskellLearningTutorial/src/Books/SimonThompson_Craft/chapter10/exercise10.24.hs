type Text = String


whitespace = ['\n', '\t', ' ']



-- precondition  - expecting no whitespace before the text.
dropWord :: Text -> Text
dropWord [] = []
dropWord (x:xs)
    | elem x whitespace = (x:xs) -- equals if char x is ' ' then return all so far.
    | otherwise         = dropWord xs


dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil p [] = []
dropUntil p (x:xs)
    | p x = (x:xs)
    | otherwise = dropUntil p xs


dropWord' :: Text -> Text
dropWord' xs = dropUntil p xs
    where p x = elem x whitespace
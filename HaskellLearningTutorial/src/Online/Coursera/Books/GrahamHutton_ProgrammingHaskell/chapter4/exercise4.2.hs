
{-Consider a function safetail :: [ a ] → [ a ] that behaves as
the library function tail , except that safetail maps the empty
list to itself, whereas tail produces an error in this case.
Deﬁne safetail using:
(a) a conditional expression;
(b) guarded equations;
(c) pattern matching.
Hint: make use of the library function null .
-}


-- c) pattern matching
safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

-- a) conditional
safeTail' :: [a] -> [a]
safeTail' xs = if null xs then [] else tail xs

-- b) guards
safeTail'' :: [a] -> [a]
safeTail'' xs
    | null xs   = []
    | otherwise = tail xs


safeTail3 :: [a] -> [a]
safeTail3
    = \xs -> case xs of
                []     -> []
                (_:xs) -> xs

main = do
    print ([safeTail [1,2,3,4,5], safeTail [1], safeTail []])
    print ([safeTail' [1,2,3,4,5], safeTail' [1], safeTail' []])
    print ([safeTail'' [1,2,3,4,5], safeTail'' [1], safeTail'' []])
    print ([safeTail3 [1,2,3,4,5], safeTail3 [1], safeTail3 []])
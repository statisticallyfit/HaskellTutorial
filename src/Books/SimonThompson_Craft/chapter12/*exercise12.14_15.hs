import Prelude hiding ((<*>))


type RegExp = String -> Bool

epsilon :: RegExp
epsilon = (== "")

char :: Char -> RegExp
char ch = (== [ch])

(|||) :: RegExp -> RegExp -> RegExp
e1 ||| e2 = \x -> e1 x || e2 x


-- note splits ("Spy") means:
-- ==> [("", "Spy"), ("S", "py"), ("Sp", "y"), ("Spy", "")]
-- note run through all splits; test whether both regexpressions match
-- the patterns. Need both matches to succeed (and). Then tell if there was any
-- success in the list (or).
-- HELP understand evaluation
(<*>) :: RegExp -> RegExp -> RegExp
e1 <*> e2 = \x -> or [e1 y && e2 z | (y, z) <- splits x ]

-- HELP understand evaluation
(<**>) :: RegExp -> RegExp -> RegExp
e1 <**> e2 = \x -> or [e1 y && e2 z | (y, z) <- tail $ splits x ]

-- note means to match (p)*: either it matches zero times (epsilon) or
-- match p followed by (p)*.
-- HELP understand how this doesn't go in infinite loop
star :: RegExp -> RegExp
star p = epsilon ||| (p <**> star p)
-- using <*> may give infinite recursion because first pattern ("","word")
-- will match.


-- note splits "Spy" ===> [("", "Spy"), ("S", "py"), ("Sp", "y"), ("Spy", "")]
splits :: [a] -> [([a], [a])]
splits xs = [ splitAt n xs | n <- [0 .. (length xs)]]




--- exercise 14 -------------------------------------------------------------------

a, b :: RegExp
a = char 'a'
b = char 'b'

main = do
    print $ star ((a ||| b) <*> (a ||| b)) "ab"
    print $ star ((a ||| b) <*> (a ||| b)) "ba"
    print $ star ((a ||| b) <*> (a ||| b)) "a "
--- exercise 15 -------------------------------------------------------------------
    -- HELP HELP understand what this does.
    print $ (star(star ((a ||| b) <*> (a ||| b)))) ""
    print $ (star(star ((a ||| b) <*> (a ||| b)))) "ba"
    print $ (star(star ((a ||| b) <*> (a ||| b)))) "ab"





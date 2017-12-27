import Prelude hiding (return)
import Data.Char




{-
NOTE
class Applicative m => Monad m where

    -- note takes result from operatoin on left side and puts it as arg to
    the operation on the right side
    (>>=) :: m a -> (a -> m b) -> m b

    -- note sequences two operations, discards result of first action
    (>>) :: m a -> m b -> m b

    return :: a -> m a
-}

{-
NOTE
fmap f xs  ======= xs >>= return . f

-}

{-
NOTE

fmap  :: Functor f     =>   (a -> b) -> f a        -> f b
(<*>) :: Applicative f => f (a -> b) -> f a        -> f b
(>>=) :: Monad f       => f a        -> (a -> f b) -> f b

-}



-- help todo how to write Show instance for a function? (S -> [a, S])
type Parser a = String -> [(a, String)]


{-
NOTE
The function return takes a type (v) and then a String (because Parser a means
(String -> [(a, String)] and then returns [(a, String)].

:t (return 1) "abc"
(return 1) "abc" :: Num a => [(a, String)]
-}

return   :: a -> Parser a
return v = \inp -> [(v, inp)]


failure :: Parser a
failure = \inp -> []


-- fails if input string is empty and succeeds with first char as result
item :: Parser Char
item = \inp -> case inp of
                [] -> []
                (x:xs) -> [(x, xs)]
-- help: todo: meaning of function args? Why doesn't it state inp type in args types?

-- abstractization
parse       :: Parser a -> String -> [(a, String)]
parse p inp = p inp

-----------------------------------------------------------------------------------
-- NOTE
-- (>>=) ::                   m    a -> (a ->   m    b) ->    m   b
-- (>>=) :: Monad Parser => Parser a -> (a -> Parser b) -> Parser b
-- key Read: "and then"
(>>=)   :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of
                    []         -> []
                    [(v, out)] -> parse (f v) out


-- parser p returns the first and third characters as a pair
-- help: todo why doesn't this work?
{-

p :: Parser (Char, Char)
p = do x <- item
       item -- see second item is discarded
       y <- item
       return (x,y)
-}



-- test: parse (item ++ return 'd') "abc" ---> [('a', "bc")]
-- test: parse (failure +++ return 'd') "abc"
-- test: parse (failure +++ failure) "abc"
-- choice operator: if first parser fails apply the second.
-- key Read: "or else"
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
                        []         -> parse q inp
                        [(v, out)] -> [(v, out)]



--sat   :: (Char -> Bool) -> Parser Char
-- HELP why different types?
-- sat :: ([(Char, String)] -> Bool) -> String -> [([(Char, String)], String)]
{-sat p = do x <- item
           if p x then return x else failure-}

sat' p xs = case item xs of
                [] -> failure xs
                [(x, xs)] -> if p x then return x else failure xs

-- derived primitives:parser sat p for single characters that satisfy predicate p
{-}

digit :: Parser Char
digit = sat isDigit


lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphaNum :: Parser Char
alphaNum = sat isAlphaNum

char   :: Char -> Parser Char
char x = sat (== x)


string        :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)
-}
main = do
    print $ return 1 "abc"
    --print $ failure "abc" -- help: todo: how to define a show?
    print $ item "abc"
    print $ parse (return 1) "abc"  -- help: todo: why error if parantheses not around return 1?
    --print $ parse failure "abc" -- todo: show thingy
    print $ parse item ""
    print $ parse item "abc"
    putStrLn "" -----------------------------------------------------------------
    print $ parse (item +++ return 'd') "abc"

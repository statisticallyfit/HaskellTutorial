module Parsing where


import Data.Char
import Control.Monad (liftM, ap)
import Control.Applicative hiding (many)

infixr 5 +++

--The monad of parsers
--------------------

newtype Parser a = P (String -> [(a,String)])

{-

instance Show a => Show (Parser a) where
    show (P (s -> [(a, s)])) = "Parser " ++ show a -- "[(" ++ show a ++ ", " ++ s ++ ")]"
-}


instance Monad Parser where
    return v = P (\inp -> [(v,inp)]) -- HELP if type is a -> Parser a then what is (v)?
    -- (>>=) ::                   m    a -> (a ->   m    b) ->    m   b
    -- (>>=) :: Monad Parser => Parser a -> (a -> Parser b) -> Parser b
    p >>= f  = \inp -> case parse p inp of
                            [] -> []
                            [(v, out)] -> parse (f v) out
    --Parser (\cs -> concat [parse (f a) cs'| (a, cs') <- parse p cs])
    -- help: is this code right? copied it from stackoverflow


instance Functor Parser where    -- help - don't know what this means, just posted it
    fmap = liftM                       -- for the code to work

instance Applicative Parser where  -- help
    pure = return
    (<*>) = ap

{-
instance MonadPlus Parser where
    mzero       = P (\inp -> [])
    p `mplus` q = P (\inp -> case parse p inp of
                                    []        -> parse q inp
                                    [(v,out)] -> [(v,out)])

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero
-}

--Basic parsers
-------------

failure                       :: Parser a -- String -> [(a,String)]
failure                       = P (\inp -> [])
-- NOTE put this code when you get MonadPlus to work
--mzero

item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                                []     -> []
                                                (x:xs) -> [(x,xs)])

parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

--Choice
------

(+++)                         :: Parser a -> Parser a -> Parser a
p +++ q                       = \inp -> case parse p inp of
                                            [] -> parse q inp
                                            [(v, out)] -> [(v, out)]

-- NOTE: use this code when  you get MonadPlus to work
-- p `mplus` q

--Derived primitives
------------------

sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

digit                         :: Parser Char
digit                         =  sat isDigit

lower                         :: Parser Char
lower                         =  sat isLower

upper                         :: Parser Char
upper                         =  sat isUpper

letter                        :: Parser Char
letter                        =  sat isAlpha

alphanum                      :: Parser Char
alphanum                      =  sat isAlphaNum

char                          :: Char -> Parser Char
char x                        =  sat (== x)

string                        :: String -> Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

many                          :: Parser a -> Parser [a]
many p                        =  many1 p +++ return []

many1                         :: Parser a -> Parser [a]
many1 p                       =  do v  <- p
                                    vs <- many p
                                    return (v:vs)

ident                         :: Parser String
ident                         =  do x  <- lower
                                    xs <- many alphanum
                                    return (x:xs)

nat                           :: Parser Int
nat                           =  do xs <- many1 digit
                                    return (read xs)

int                           :: Parser Int
int                           =  error "You must implement int"

space                         :: Parser ()
space                         =  do many (sat isSpace)
                                    return ()

comment                       :: Parser ()
comment                       = error "You must implement comment"

expr                          :: Parser Int
expr                          = error "You must implement expr"

--Ignoring spacing
----------------

token                         :: Parser a -> Parser a
token p                       =  do space
                                    v <- p
                                    space
                                    return v

identifier                    :: Parser String
identifier                    =  token ident

natural                       :: Parser Int
natural                       =  token nat

integer                       :: Parser Int
integer                       =  token int

symbol                        :: String -> Parser String
symbol xs                     =  token (string xs)



main = do
    print ""
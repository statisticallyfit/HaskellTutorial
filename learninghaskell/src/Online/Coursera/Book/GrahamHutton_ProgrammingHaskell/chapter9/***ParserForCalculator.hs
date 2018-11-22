import Control.Monad
import Data.Char


-- parser for calculator
newtype Parser a = P (String -> [(a,String)])



instance Functor Parser where    -- help - don't know what this means, just posted it
    fmap = liftM                       -- for the code to work

instance Applicative Parser where  -- help
    pure = return
    (<*>) = ap
    -- (<|>) = mplus
    -- empty = mzero


instance Monad Parser where
    return v =  P (\inp -> [(v,inp)])
    p >>= f =  P (\inp -> case parse p inp of
                            [] -> []
                            [(v,out)] -> parse (f v) out)


instance MonadPlus Parser where
    mzero =  P (\inp -> [])
    p `mplus` q =  P (\inp -> case parse p inp of
                                [] -> parse q inp
                                [(v,out)] -> [(v,out)])


failure :: Parser a
failure =  mzero

item :: Parser Char
item =  P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x,xs)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp =  p inp

(+++) :: Parser a -> Parser a -> Parser a
p +++ q =  p `mplus` q

sat :: (Char->Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             y <- many p
             return (x:y)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- many1 digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = do token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

eval :: String -> Int
eval xs = case parse expr xs of
            [(n,[])] -> n
            [(_,out)] -> error ("unused input " ++ out)
            [] -> error "invalid input"

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
            +++ do symbol "-"
                   e <- expr
                   return (t - e)
            +++ return t

term :: Parser Int
term = do p <- power
          do symbol "*"
             t <- term
             return (p * t)
            +++ do symbol "/"
                   t <- term
                   return (p `div` t)
            +++ return p

power :: Parser Int
power = do f <- factor
           do symbol "^"
              p <- power
              return (f ^ p)
             +++ return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         +++ natural



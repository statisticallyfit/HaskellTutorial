-- Exercise 1 --------------------------------------------------------------------
data Operator = Plus | Minus | Times | Div
            deriving (Show, Eq)


opToChar       :: Operator -> Char
opToChar Plus  = '+'
opToChar Minus = '-'
opToChar Times = '*'
opToChar Div   = '/'

opToStr :: Operator -> String
opToStr Plus  = "+"
opToStr Minus = "-"
opToStr Times = "*"
opToStr Div   = "/"


data Token = TokOp Operator
            | TokIdent String
            | TokNum Int
            deriving (Show, Eq)

showContent                :: Token -> String
showContent (TokOp op)     = opToStr op
showContent (TokIdent str) = str
showContent (TokNum i)     = show i


token :: Token
token = TokIdent "x"

{-}main = do
    putStrLn $ showContent token
    print token
    putStrLn $ showContent (TokOp Div)
    putStrLn $ showContent (TokNum 23)
    print (TokNum 23)
-}
-- Exercise 2 --------------------------------------------------------------------
data Point = Pt Double Double
                deriving Show

inc          :: Point -> Point
inc (Pt x y) = Pt (x+1) (y+1)

p :: Point
p = Pt (-1) 3

--main = print $ inc p

-- Exercise 3 --------------------------------------------------------------------

inc2 :: (Int, Int) -> (Int, Int)
inc2 (x,y) = ((x+1), (y+1))

p2 :: (Int, Int)
p2 = (-1,3)

main = print $ inc2 p2


-- Exercise 4 --------------------------------------------------------------------



-- Exercise 5 --------------------------------------------------------------------




-- Exercise 6 --------------------------------------------------------------------


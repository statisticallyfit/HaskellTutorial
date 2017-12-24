import Data.Char
import Prelude hiding (span, {-return, fail-})
import qualified Data.Map as M
import Control.Applicative hiding (Const )
import Control.Monad (liftM, ap)

{-
data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokAssign
           | TokLParen -- (
           | TokRParen -- )
           | TokIdent String
           | TokNum Double
           | TokEnd
    deriving (Show, Eq)


data Expression

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div


tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
    | elem c "+-*/" = TokOp (operator c) : tokenize cs
    | c == '='  = TokAssign : tokenize cs
    | c == '('  = TokLParen : tokenize cs
    | c == ')'  = TokRParen : tokenize cs
    | isDigit c = number c cs
    | isAlpha c = identifier c cs
    | isSpace c = tokenize cs  -- if space, just skip, don't need TokSpace anymore.
    | otherwise = error $ "Cannot tokenize " ++ [c]


identifier :: Char -> String -> [Token]
identifier c cs = let (str, cs') = span isAlphaNum cs in
                  TokIdent (c:str) : tokenize cs'



number :: Char -> String -> [Token]
number c cs =
   let (digs, cs') = span isDigit cs in     -- note: definition of span in #7
   TokNum (read (c : digs)) : tokenize cs'



span :: (a -> Bool) -> [a] -> ([a], [a])
span pred str = spanAcc [] str
    where  -- define he.lper 'spanAcc"
        spanAcc acc []     = (acc, [])
        spanAcc acc (c:cs) | pred c = (c:acc', cs')
                           | otherwise = (acc, c:cs)--spanAcc acc cs
                           where (acc', cs') = spanAcc acc cs
                           -- note: fixed it was "otherwise = (acc, c:cs)" before


{-}
tokenize :: String -> [Token]
tokenize = map tokenizeChar

-- note: the curried version (dividing both sides by the argument str)

-- note: the uncurried version
-- tokenize str = map tokenizeChar str


tokenizeChar :: Char -> Token
tokenizeChar c | elem c "+-*/" = TokOp (operator c)
               | isDigit c  = TokNum (digitToInt c)
               | isAlpha c  = TokIdent [c]
               | isSpace c  = TokSpace
               | otherwise  = error $ "Cannot tokenize " ++ [c]


deSpace :: [Token] -> [Token]
deSpace = filter (\t -> t /= TokSpace)
-}



---- parser ----

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
    deriving Show


lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (t:ts) = t

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts



expression :: [Token] -> (Tree, [Token])
expression toks =
   let (termTree, toks') = term toks
   in
      case lookAhead toks' of
         (TokOp op) | elem op [Plus, Minus] ->
            let (exTree, toks'') = expression (accept toks')
            in (SumNode op termTree exTree, toks'')
         TokAssign ->
            case termTree of
               VarNode str ->
                  let (exTree, toks'') = expression (accept toks')
                  in (AssignNode str exTree, toks'')
               _ -> error "Only variables can be assigned to"
         _ -> (termTree, toks')



term :: [Token] -> (Tree, [Token])
term toks =
   let (facTree, toks') = factor toks
   in
      case lookAhead toks' of
         (TokOp op) | elem op [Times, Div] ->
            let (termTree, toks'') = term (accept toks')
            in (ProdNode op facTree termTree, toks'')
         _ -> (facTree, toks')



factor :: [Token] -> (Tree, [Token])
factor toks =
   case lookAhead toks of
      (TokNum x)     -> (NumNode x, accept toks)
      (TokIdent str) -> (VarNode str, accept toks)
      (TokOp op) | elem op [Plus, Minus] ->
            let (facTree, toks') = factor (accept toks)
            in (UnaryNode op facTree, toks')
      TokLParen      ->
         let (expTree, toks') = expression (accept toks)
         in
            if lookAhead toks' /= TokRParen
            then error "Missing right parenthesis"
            else (expTree, accept toks')
      _ -> error $ "Parse error on token: " ++ show toks



parse :: [Token] -> Tree
parse toks = let (tree, toks') = expression toks
             in
               if null toks'
               then tree
               else error $ "Leftover tokens: " ++ show toks'







-- evaluator -------------------------


type SymTab = M.Map String Double


-- substituted String for Tree in signature -- help
lookUp :: String -> SymTab -> Either String (Double, SymTab)
lookUp str symTab =
    case M.lookup str symTab of
        Just v  -> return (v, symTab)
        Nothing -> fail ("Undefined variable " ++ str)


-- inserts new string/value pair into symbol table then we return new value of it.
-- M.insert will overwrite previous pair if key already exists in map.

addSymbol :: String -> Double -> SymTab -> Either String ((), SymTab)
addSymbol str val symTab =
    let symTab' = M.insert str val symTab
    in Right ((), symTab')


-- thread in the symbol table through every call to evaluate so that
-- we always have access to it
-- When evaluate needs to modify the symbol table, it constructs a new copy
-- of the symbol table with the modification included, returning the new copy.


{-
note 1: here is the structure before adding error-checks:

evaluate (SumNode op left right) symTab =
    let (lft, symTab') = evaluate left symTab
        (rgt, symTab'') = evaluate right symTab'
    in
        case op of
          Plus  -> (lft + rgt, symTab'')
          Minus -> (lft - rgt, symTab'')



note 2: here is the structure before abstracting the Either pattern in
using bindE function:

evaluate (SumNode op left right) symTab =
    case evaluate left symTab of
    Left msg -> Left msg
    Right (lft, symTab') ->
        case evaluate right symTab' of
        Left msg -> Left msg
        Right (rgt, symTab'') ->
            case op of
            Plus  -> Right (lft + rgt, symTab'')
            Minus -> Right (lft - rgt, symTab'')


note 3: here is the structure using the monad evaluator in SumNode without the
do notation

evaluate (SumNode op left right) symTab =
    evaluate left symTab >>= \(lft, symTab') ->
        evaluate right symTab' >>= \(rgt, symTab'') ->
            case op of
              Plus  -> return (lft + rgt, symTab'')
              Minus -> return (lft - rgt, symTab'')


note 4: SumNode with monad WITH do notation

evaluate (SumNode op left right) symTab = do
    (lft, symTab')  <- evaluate left symTab
    (rgt, symTab'') <- evaluate right symTab'
    case op of
      Plus  -> return (lft + rgt, symTab'')
      Minus -> return (lft - rgt, symTab'')


      NOTE: do notation is syntactic sugar, hides the binding >>= between the
      lines. It converts the rest of the code to continuations and treats the
      args to lambdas as if they were local variables to be assigned to.
-}


evaluate :: Tree -> SymTab -> Evaluator (Double, SymTab)

-- help: how does it get the value symTab' and lft and rgt? they haven't been
-- assigned/created yet have they?
evaluate (SumNode op left right) symTab =
    evaluate left symTab >>= \(lft, symTab') ->
        evaluate right symTab' >>= \(rgt, symTab'') ->
            case op of
                Plus  -> return (lft + rgt, symTab'')
                Minus -> return (lft - rgt, symTab'')

evaluate (ProdNode op left right) symTab =
    case evaluate left symTab of
    Left msg -> Left msg
    Right (lft, symTab') ->
        case evaluate right symTab' of
        Left msg -> Left msg
        Right (rgt, symTab'') ->
            case op of
            Times -> Right (lft * rgt, symTab)
            Div   -> Right (lft / rgt, symTab)

evaluate (UnaryNode op tree) symTab =
    bindE (evaluate tree symTab)
          (\(x, symTab') ->
                case op of
                    Plus  -> return (x, symTab')
                    Minus -> return (-x, symTab'))

evaluate (NumNode x) symTab = Right (x, symTab)

evaluate (VarNode str) symTab = lookUp str symTab

evaluate (AssignNode str tree) symTab =
    case evaluate tree symTab of
    Left msg -> Left msg
    Right (v, symTab') ->
        case addSymbol str v symTab' of
        Left msg -> Left msg
        Right (_, symTab'') -> Right (v, symTab'')




-- Extracting the pattern 1. evaluate tree symTab and pattern 2. case op of ...
-- into a function.
{-
note:
bindE :: Either String (Double, SymTab)
      -> ((Double, SymTab) -> Either String (Double, SymTab))
      -> Either String (Double, SymTab)

      can be further abstracted into using types a and b if we use addSymbol
      which returns ((), SymTab) and not (Double, SymTab)
-}
bindE :: Either String a
      -> (a -> Either String b)
      -> Either String b
bindE ev k =
    case ev of
        Left msg -> Left msg
        Right v  -> k v

-- note: v is the argument (x, symTab')
-- note: k is kontinuation - the lambda argument - the second case op of statement
-- from the previous structure:
{-
evaluate (UnaryNode op tree) symTab =
    case evaluate tree symTab of
    Left msg -> Left msg
    Right (x, symTab') ->
        case op of
        Plus  -> Right ( x, symTab')
        Minus -> Right (-x, symTab')
-}


-- note: abstracting out the Left/RIght error/correct messages
return :: a -> Either String a
return x = Right x

fail :: String -> Either a
fail msg = Left msg





-- note:
{-
Monad is defined by a type constructor (type parametrized by another type)
and two functions (bind and return, and optionally fail)
Monad is a typeclasss - we need to instatiate Monad with our Evaluator.
Need to define bind, return, and fail (optionally).
bind function is written >>= (infix)
-}

-- note: newtype is mix between type and data.
-- used to define data types with only one data constructor that takes just one arg.
newtype Evaluator a = Eval (Either String a)

-- so
{-
evaluate :: Tree -> SymTab -> Either String (Double, SymTab)
becomes
evaluate :: Tree -> SymTab -> Evaluator (Double, SymTab)
-}



instance Monad Evaluator where
    (Eval ev) >>= k =                      -- defining the bind >>= operator
        case ev of
            Left msg -> Eval (Left msg)
            Right v  -> k v
    return v = Eval (Right v)              -- defining return
    fail msg = Eval (Left msg)             -- defining fail



{-
main = do
   loop (M.fromList [("pi", pi)])

loop symTab = do
   str <- getLine
   if null str
   then
      return ()  -- help will have to fix this - is this Prelude.return or our return?
   else
      let toks = tokenize str
          tree = parse toks
      in
          case evaluate tree symTab of
          Left msg -> do
              putStrLn $ "Error: " ++ msg
              loop symTab -- use old symTab
          Right (v, symTab') -> do
              print v
              loop symTab'

-}


-- help: error to fix
src/BasicsOfHaskell/10.ErrorHandling.hs:367:5:
    `return' is not a (visible) method of class `Monad'

src/BasicsOfHaskell/10.ErrorHandling.hs:368:5:
    `fail' is not a (visible) method of class `Monad'





-}


----------- Example 1 TypeClasses ---------------------------------------------------
{-
class Valuable a where
    eval :: a -> Double

data Expr = Const Double | Add Expr Expr

instance Valuable Expr where -- Expr is now explicitly an instance of Valuable
    eval (Const x) = x
    eval (Add lft rgt) = eval lft + eval rgt


instance Valuable Bool where
    eval True = 1
    eval False = 0


test :: Valuable a => a -> IO() -- arg 'a' has to be instance of Valuable
test v = print $ eval v


expr :: Expr
expr = Add (Const 2) (Add (Const 1.5) (Const 2.5))

main = do
    test expr
    test True

-}


-- Example 2: Typeclasses - solution to the expression problem ----------------------


class Expr a

data Const   = Const Double -- these two Const names are separate
data Add a b = Add a b
data Mul a b = Mul a b

instance Expr Const
instance (Expr a, Expr b) => Expr (Add a b)
instance (Expr a, Expr b) => Expr (Mul a b)
-- note: 'a' and 'b' must be Expr for Add a b to be Expr


-- note: the only way to define evaluate for both Add and Const data types
-- is to overload evaluate function. Possible only if evaluate is part of a
-- typeclass Valuable.

class (Expr e) => Valuable e where  -- note: only expressions can be evaluated.
    evaluate :: e -> Double         -- help: connecting the Expr and Valuable? ?

-- making evaluate work for Const and Add and Mul
instance Valuable Const where
    evaluate (Const x) = x
instance (Valuable a, Valuable b) => Valuable (Add a b) where
    evaluate (Add lft rgt) = evaluate lft + evaluate rgt
instance (Valuable a, Valuable b) => Valuable (Mul a b) where
    evaluate (Mul lft rgt) = evaluate lft * evaluate rgt



-- note: add pretty printing function to expressions
-- Trick: make pretty a member of new class Pretty and make all expression types
-- its instances

class (Expr e) => Pretty e where
    pretty :: e -> String

instance Pretty Const where
    pretty (Const x) = show x
instance (Pretty a, Pretty b) => Pretty (Add a b) where
    pretty (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
instance (Pretty a, Pretty b) => Pretty (Mul a b) where
    pretty (Mul x y) = pretty x ++ " * " ++ pretty y


expr = Mul (Const 2) (Add (Const 1.5) (Const 2.5))





--- The Monad Typeclass ---------------------------------------------------------

{-
class Monad m where
    (>>=)  :: m a -> (a -> m b ) -> m b
    (>>)   :: m a -> m b -> m b
    return :: a -> m a
    fail   :: String -> m a

    mv >> k = mv >>= \_ -> k
    fail s = error s

    note: not a typeclass that unifies types; rather it unifies type
    constructors.
    'm' is type constructor and requires another type parameter to become a type.
    'a' and 'b' are type variables, acted on by 'm'.
-}











-- Exercise 1 --------------------------------------------------------------------
-- whynot monad
data WhyNot a = Nah | Sure a
    deriving Show

instance Monad WhyNot where
    Sure x >>= k = k x  -- help: what does >>= mean here and what does k mean?
    Nah    >>= _ = Nah
    return x     = Sure x
    fail _       = Nah


instance Functor WhyNot where    -- help - don't know what this means, just posted it
    fmap = liftM                       -- for the code to work
instance Applicative WhyNot where  -- help
    pure = return
    (<*>) = ap

safeRoot :: Double -> WhyNot Double
safeRoot x =
    if x >= 0 then
        return (sqrt x)
    else
        fail "Boo! won't get printed"


test :: Double -> WhyNot Double
test x = do
    y <- safeRoot x
    z <- safeRoot (y-4) -- help: does it print Nah because it got negative here?
    w <- safeRoot z
    return w


{-}
main = do
    print $ test 9
    print $ test 400
-}


-- Exercise 2 --------------------------------------------------------------------

newtype Trace a = Trace ([String], a)

instance Monad Trace where
    return x = Trace ([], x)
    (Trace (lst, x)) >>= k = Trace (lst ++ lst', y)
                             where Trace (lst', y) = k x
                             -- help - meaning?

instance Functor Trace where    -- help - don't know what this means, just posted it
    fmap = liftM                       -- for the code to work
instance Applicative Trace where  -- help
    pure = return
    (<*>) = ap


put :: Show a => String -> a -> Trace ()
put msg v = Trace ([msg ++ " " ++ show v], ())


fact :: Integer -> Trace Integer
fact n = do
    put "fact" n
    if n == 0
    then return 1
    else do
        m <- fact (n-1) -- help: why separate why not put return (n*fact(n-1)) ?
        return (n*m)


{-
main = let Trace (lst, m) = fact 3
       in do
            print lst
            print m

-}

-- Exercise 4 --------------------------------------------------------------------

data Color = White | Black
    deriving (Show, Eq)

data Pawn = Pawn Color (Int, Int) --tuple is position

class Piece a where
    color :: a -> Color
    pos   :: a -> (Int, Int)
    moves :: a -> [(Int, Int)]

instance Piece Pawn where
    color (Pawn c _) = c
    pos (Pawn _ pos) = pos
    moves pwn = if color pwn == White                        -- help understand logic
                then mvs (pos pwn)                           -- of the game here.
                else map refl (mvs $ refl (pos pwn))
          where
            refl (x,y) = (x, 7-y)
            mvs (x,y)  = if y == 1
                         then [(x, y+1), (x, y+2)]
                         else if y == 7
                            then []
                            else [(x, y+1)]

pieces = [Pawn White (3,1), Pawn Black (4,1),
          Pawn White (0,7), Pawn Black (5,0)]


main = print $ map moves pieces


{-
main = do
    putStrLn $ pretty expr
    print $ evaluate expr

-}
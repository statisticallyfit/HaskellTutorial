import Data.Char
import Prelude hiding (span)
import qualified Data.Map as M

import Control.Monad (liftM, ap)
import Control.Applicative


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


-- Exercise 3 for 10.Error handling
{-
instance Show Operator where
    show Plus = " + "
    show Minus = " - "
    show Times = " * "
    show Div = " / "

instance Show Tree where
    show (SumNode op lft rgt) = "(" ++ show lft ++ show op ++ show rgt ++ ")"
    show (ProdNode op lft rgt) = show lft ++ show op ++ show rgt
    show (AssignNode str tree) = str ++ " = " ++ show tree
    show (UnaryNode op tree) = show op ++ show tree
    show (NumNode x) = show x
    show (VarNode str) = str
-}


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

-- goes on a streak of alphas (letters) and gathers them up, stops when not an alpha.
identifier :: Char -> String -> [Token]
identifier c cs = let (str, cs') = span isAlphaNum cs in
                  TokIdent (c:str) : tokenize cs'

-- reads a streak of numbers
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
                           -- help: why can't it be "otherwise = spanAcc acc cs" ???


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



-- thread in the symbol table through every call to evaluate so that
-- we always have access to it
-- When evaluate needs to modify the symbol table, it constructs a new copy
-- of the symbol table with the modification included, returning the new copy.


{-
note: here is the structure before adding error-checks

evaluate (SumNode op left right) symTab =
    let (lft, symTab') = evaluate left symTab
        (rgt, symTab'') = evaluate right symTab'
    in
        case op of
          Plus  -> (lft + rgt, symTab'')
          Minus -> (lft - rgt, symTab'')

note: here is the structure when doing the EIther monad

evaluate :: Tree -> SymTab -> Either String (Double, SymTab)

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



note: structure when currying

(second) reason is that this form brings us closer to our goal of
abstracting away the tedium of symbol-table passing. Symbol table passing
is what "actions" are supposed to do; evaluate should only construct the
tracks for the symbol-table train.

evaluate :: Tree -> (SymTab -> (Double, SymTab))
evaluate (UnaryNode op tree) =
    \symTab ->
        let act = evaluate tree
            (x, symTab') = {-hi-}act symTab{-/hi-}
        in case op of
            Plus  -> ( x, symTab')
            Minus -> (-x, symTab')




note ------------------------------------------------------------------------------
structure of monad evaluator before state monad

instance Monad Evaluator where
    (Ev ev) >>= k =
        case ev of
          Left msg -> Ev (Left msg)
          Right v -> k v
    return v = Ev (Right v)
    fail msg = Ev (Left msg)
-}


type SymTab = M.Map String Double

newtype Evaluator a = Ev (SymTab -> (a, SymTab))

instance Monad Evaluator where
    (Ev act) >>= k = Ev $
        \symTab ->
            let (x, symTab') = act symTab
                (Ev act') = k x
            in act' symTab'
    return x = Ev (\symTab -> (x, symTab))


instance Functor Evaluator where    -- help - don't know what this means, just posted it
    fmap = liftM                       -- for the code to work

instance Applicative Evaluator where  -- help
    pure = return
    (<*>) = ap



-- helphelphelp: how to understand how the monad operation applies through the <-

evaluate :: Tree -> Evaluator Double -- Ev (SymTab -> (Double, SymTab))

evaluate (SumNode op left right) = do
    lft <- evaluate left -- note: evaluate left = act that takes symTab as argument
    rgt <- evaluate right
    case op of
       Plus  -> return $ lft + rgt
       Minus -> return $ lft - rgt

evaluate (ProdNode op left right) = do
    lft <- evaluate left
    rgt <- evaluate right
    case op of
       Times -> return $ lft * rgt
       Div   -> return $ lft / rgt

evaluate (UnaryNode op tree) = do
    x <- evaluate tree
    case op of
       Plus  -> return x
       Minus -> return (-x)

evaluate (NumNode x) = return x

evaluate (VarNode str) = lookUp str

evaluate (AssignNode str tree) = do
    v <- evaluate tree
    addSymbol str v




lookUp :: String -> Evaluator Double
lookUp str = Ev $ \symTab ->
    case M.lookup str symTab of
      Just v  -> (v, symTab)
      Nothing -> error $ "Undefined variable " ++ str

addSymbol :: String -> Double -> Evaluator Double
addSymbol str val = Ev $ \symTab ->
    let symTab' = M.insert str val symTab
    in  (val, symTab')




main = do
   loop (M.fromList [("pi", pi)])

loop symTab = do
   putStr "expr > "
   str <- getLine
   if null str
   then
      return ()
   else
      let toks = tokenize str
          tree = parse toks
          Ev act = evaluate tree
          (val, symTab') = act symTab
      in do
          print val
          loop symTab'
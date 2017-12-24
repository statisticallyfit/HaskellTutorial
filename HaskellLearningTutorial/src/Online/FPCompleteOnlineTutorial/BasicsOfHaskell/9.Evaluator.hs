import Data.Char
import Prelude hiding (span)
import qualified Data.Map as M -- to call M.lookup
-- qualified since the lookup func contradicts with the Prelude one.


data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokAssign
           | TokLParen -- (
           | TokRParen --  )
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


type symTab = M.Map String Double



-- thread in the symbol table through every call to evaluate so that
-- we always have access to it
-- When evaluate needs to modify the symbol table, it constructs a new copy
-- of the symbol table with the modification included, returning the new copy.

evaluate :: Tree -> SymTab -> (Double, SymTab)

evaluate (SumNode op left right) symTab =
    let (lft, symTab') = evaluate left symTab
        (rgt, symTab'') = evaluate right symTab'
    in
        case op of
          Plus  -> (lft + rgt, symTab'')
          Minus -> (lft - rgt, symTab'')

evaluate (ProdNode op left right) symTab =
    let (lft, symTab') = evaluate left symTab
        (rgt, symTab'') = evaluate right symTab'
    in
        case op of
          Times -> (lft * rgt, symTab'')
          Div   -> (lft / rgt, symTab'')

evaluate (UnaryNode op tree) symTab =
    let (x, symTab') = evaluate tree symTab
    in case op of
         Plus  -> (x, symTab')
         Minus -> (-x, symTab')

evaluate (NumNode x) symTab = (x, symTab)

evaluate (VarNode str) symTab = lookUp str symTab

evaluate (AssignNode str tree) symTab =
    let (v, symTab')  = evaluate tree symTab
        (_, symTab'') = addSymbol str v symTab'
    in (v, symTab'')




-- note note note: we are threading this complexity to later show it can be
-- done more easily with monads.


-- substituted String for Tree in signature -- help
lookUp :: String -> SymTab -> (Double, SymTab)
lookUp str symTab =
    case M.lookup str symTab of
        Just v -> (v, symTab)
        Nothing -> error $ "Undefined variable " ++ str


-- inserts new string/value pair into symbol table then we return new value of it.
-- M.insert will overwrite previous pair if key already exists in map.
addSymbol :: String -> Double -> SymTab -> ((), SymTab)
addSymbol str val symTab =
    let symTab' = M.insert str val symTab
    in ((), symTab')






main = do
    loop (M.fromList [("pi", pi)])

loop symTab do
    str <- getLine
    if null str
    then return ()
    else
        let toks = tokenize str
            tree = parse toks
            (val, symTab') = evaluate tree symTab
        in do
            print val
            loop symTab' -- note: we pass symTab to evaluate, keeping passing modified table.


    --(print . evaluate . parse . tokenize) "x1 = -15 / (2 + x2)"


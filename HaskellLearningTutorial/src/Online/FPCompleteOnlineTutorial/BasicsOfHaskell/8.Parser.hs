import Data.Char
import Prelude hiding (Word)

{-}
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



{- note
1. An additive expression that starts with a Term followed by either plus or minus,
 followed by another expression. This is a typical recursive definition for parsing
 expressions of the type Term + Term - Term ... etc.. Example: x - 5 + y.
2. An assignment of the form: Identifier equals Expression. For simplicity,
I chose to treat an assignment as an expression, as it is in C++, rather than a
separate statement. Example: x = 2 + 2
3. Finally, a lonely Term is also considered an expression. Example: 44.

Terms are more tightly bound than expressions, corresponding to higher precedence
of multiplicative vs. additive operators. We'll consider two forms or terms:

1. Factor followed by a multiplication or division sign, followed by another term.
This production corresponds to terms of the form Factor * Factor / Factor ....
Example: 2 * x / 2.
2. A Term could also be a lonely Factor. Example: 44.
Finally, the most tightly bound production is that of the Factor, which can be one
of:

1. A Number, like 147
2. An Identifier (variable name), like x11
3. A unary plus or minus in front of a Factor, like -x or +12. You can convince
yourself that there is no ambiguity between binary and unary uses of these operators.
4. A parenthesized Expression, like (a + b/2)
-}

{-}

Expression <- Term [+-] Expression       -- example: Term + Term - Term ...
            | Identifier '=' Expression  -- example: x = 2 + 2
            | Term                       -- example: 44
Term       <- Factor [*/] Term           -- example: Factor * Factor / Factor ...
            | Factor                     -- example: 44
Factor     <- Number                     -- example: 147
            | Identifier                 -- example: x11
            | [+-] Factor                -- example: -x or +12
            | '(' Expression ')'         -- example: (a+b/2)
-- note: this is right associative for operators with same precedence so
-- 5 - 3 + 2 is (5 - (3+2)) but if defined otherwise then it will be more complicated.
-- help: todo: how to do it myself?
-}


data Tree = SumNode Operator Tree Tree
            | ProdNode Operator Tree Tree
            | AssignNode String Tree
            | UnaryNode Operator Tree
            | NumNode Double
            | VarNode String
            deriving Show


-- pure parsing functions: expression, term, factor

-- try to parse a Term by calling 'term'. Then look ahead at next token.
-- if it is a TokOp containing Plus of Minus we will create a sumNode. It it is a
-- TokAssign we will create an AssignNode. Otherwise return the lonely Term
expression :: [Token] -> (Tree, [Token])
expression toks =
    let (termTree, toks') = term toks
    in
        case lookAhead toks' of
            -- Term [+-] Expression
            (TokOp op) | elem op [Plus, Minus] ->
                let (exTree, toks'') = expression (accept toks')
                in (SumNode op termTree exTree, toks'')
            -- Identifier '=' Expression
            TokAssign ->
                case termTree of
                    VarNode str ->
                        let (exTree, toks'') = expression (accept toks')
                        in (AssignNode str exTree, toks'')
                    _ -> error "Only variables can be assigned to."
            -- Term
            _ -> (termTree, toks')

{-note: help: Notice that the assignment branch, Identifier '=' Expression, requires
that the left hand side (the termNode) be a VarNode. This condition is
checked at runtime and, if not met, results in an error.
-}

-- Basic idea: if you see a Term, then create a Factor tree and parse the
-- rest for more terms. Otherwise, if just a lonely factor, return the
-- factor tree and rest of the tokens.
term      :: [Token] -> (Tree, [Token])
term toks =
    let (facTree, toks') = factor toks
    in
        case lookAhead toks' of
            -- Factor [*/] Term
            (TokOp op) | elem op [Times, Div] ->
                let (termTree, toks'') = term (accept toks')
                in (ProdNode op facTree termTree, toks'')
            -- Factor
            _ -> (facTree, toks')



-- Basic idea: can be number,
-- identifier, factor, or expression
factor     :: [Token] -> (Tree, [Token])
factor toks =
    case lookAhead toks of
        -- Number
        (TokNum x)     -> (NumNode x, accept toks)
        -- Identifier
        (TokIdent str) -> (VarNode str, accept toks)
        -- [+-] Factor
        (TokOp op) | elem op [Plus, Minus] ->
            let (facTree, toks') = factor (accept toks)
            in (UnaryNode facTree, toks')
        -- ( Expression )
        TokLParen      ->
            let (expTree, toks') = expression (accept toks)
            in
                if lookAhead toks' /= TokRParen
                then error "Missing right parenthesis"
                else (expTree, accept toks')
        _ -> error $ "Parse error on token: " ++ show toks





-- some helper functions to access tokens. Look one token ahead and when using it,
-- call accept to remove it from the list (actually return the tail of the list)
lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (t:ts) = t

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts -- return tokens




------ PARSER ----------------------------------------------------------------------
-- Parsing starts at the top.
-- expecting expression at top level so call expression and pattern match.


parse :: [Token] -> Expression
parse toks = let (tree, toks') = expression toks
             in
                if null toks'
                then tree
                else error $ "Leftover tokens: " ++ show toks'

-}





-- help: understand this better
-- Exercise 1 ----------------------------------------------------------------------

data Token = TokLParen | TokRParen | TokEnd
    deriving (Show, Eq)

lookAhead :: [Char] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs) | c == '('  = TokLParen
                 | c == ')'  = TokRParen
                 | otherwise = error $ "Bad input: " ++ (c:cs)


accept :: [Char] -> [Char]
accept [] = error "Nothing to accept"
accept (c:cs) = cs


data Tree = Node Tree Tree | Leaf
    deriving Show

{-
Root   <- Par
Expr   <- Par Par
Par    <- '(' Expr ')'
        | '(' ')'
-}

root, expr, par :: [Char] -> (Tree, [Char])


root = par

expr toks =
    let (p, toks')   = par toks
        (p', toks'') = par toks'
    in (Node p p', toks'')

par toks =
    case lookAhead toks of
        TokLParen ->
            case lookAhead (accept toks) of
                TokRParen -> (Leaf, accept (accept toks))
                _ -> let (e, toks') = expr (accept toks)
                     in
                        if lookAhead toks' == TokRParen
                        then (e, accept toks')
                        else error $ "Missing closing paren in: " ++ show toks'
        _ -> error $ "Bad expression: " ++ show toks


parse str = let (tree, str') = root str
            in
                if null str'
                then tree
                else error $ "Unconsumed string " ++ str'


-- Exercise 2 ----------------------------------------------------------------------
-- help: understand this better.

type Word = String

sentence :: String -> [Word]
sentence "" = []
sentence str = let (w, str') = word str
               in w : sentence str'

-- todo: how does this recursion work???
word :: String -> (Word, String)
word "" = ("", "")
word (l:ls) | isSpace l = ("", ls)
            | otherwise = (l:w, ls')
                          where (w, ls') = word ls

word' str = let (w, str')  = span (not . isSpace) str
                (_, str'') = span isSpace str'
            in (w, str'')

-- Exercise 3 ----------------------------------------------------------------------
type Parser a = String -> (a, String)

several :: Parser a -> String -> [a]
several p ""  = []
several p str = let (a, str') = p str
                    as        = several p str' -- help: understand this part better.
                in a:as

num :: Parser Int
num str =
    let (digs, str') = span isDigit str
        (_, str'')   = span isSpace str'
    in (read digs, str'')



main = do
    print $ parse "(()(()()))"
    print $ sentence "Ceci n'est pas une phrase"
    print $ several num "12 4 128"
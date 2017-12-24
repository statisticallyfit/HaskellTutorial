import Data.Char
import Prelude hiding (span)
import qualified Data.Map as M
import Control.Monad (liftM, ap)
import Control.Applicative

import Control.Monad.State


--------------- 11. State Monad ----------------------------------------------------


-- abstracting out the pattern of passing SymTab

{-
**TC = type constructor we are monadizing

bind :: TC a -> (a -> TC b) -> TC b
-}


{-

note: structure before doing state monad returnS

evaluate :: Tree -> SymTab -> Evaluator (Double, SymTab)
evaluate (UnaryNode op tree) symTab = do
    (x, symTab') <- evaluate tree symTab
    case op of
        Plus  -> return ( x, symTab')


note: looking for pattern to extract...

evaluate :: Tree -> (SymTab -> (a, SymTab))
evaluate (UnaryNode op tree) =
    (\symTab ->
          let act = evaluate tree
              (x, symTab') = act symTab
          in case op of
              Plus  -> ( x, symTab')
              Minus -> (-x, symTab'))


note: structure after doing state monad returnS

evaluate :: Tree -> (SymTab -> (Double, SymTab))
evaluate (UnaryNode op tree) =
    \symTab ->
        let act          = evaluate tree
            (x, symTab') = act symTab
            k            = \x' -> case op of
                              Plus  -> returnS x'
                              Minus -> returnS (-x')
            act'         = k x
        in
            act' symTab'


note: structure after refactoring the above into bindS:

evaluate :: Tree -> (SymTab -> (Double, SymTab))
evaluate (UnaryNode op tree) =
    bindS (evaluate tree)
          (\x -> case op of
                    Plus  -> returnS x
                    Minus -> returnS (-x))


note: structure after implementing the Monad

evaluate (UnaryNode op tree) = do
    x <- evaluate tree
    case op of
       Plus  -> return x
       Minus -> return (-x)

evaluate (SumNode op left right) = do
    lft <- evaluate left
    rgt <- evaluate right
    case op of
       Plus  -> return (lft + rgt)
       Minus -> return (lft - rgt)


data Operator = Plus | Minus
data Tree = UnaryNode Operator Tree | SumNode Operator Tree Tree
type SymTab = ()
-- show
newtype Evaluator a = Ev (SymTab -> (a, SymTab))


instance Monad Evaluator where
    return x = Ev (\symTab -> (x, symTab))
    (Ev act) >>= k = Ev $ \symTab ->
        let (x, symTab') = act symTab
            (Ev act')    = k x
        in
            act' symTab'

instance Functor Evaluator where    -- help - don't know what this means, just posted it
    fmap = liftM                       -- for the code to work
instance Applicative Evaluator where  -- help
    pure = return
    (<*>) = ap

returnS :: a -> Evaluator a
returnS x = \symTab -> (x, symTab)

bindS :: Evaluator a
      -> (a -> Evaluator b)
      -> Evaluator b
bindS act k =
    \symTab ->
        let (x, symTab') = act symTab
            act' = k x
        in
            act' symTab'
-}

-- note: using do notation the state monad hides the process of threading state
-- (symTab) through the functions
{-
evaluate :: Tree -> (SymTab -> (Double, SymTab))
evaluate (UnaryNode op tree) = do
    x <- evaluate tree
    case op of
        Plus  -> return x
        Minus -> return (-x)

evaluate (SumNode op left right) = do
    lft <- evaluate left
    rgt <- evaluate right
    case op of
        Plus  -> return (lft + rgt)
        Minus -> return (lft - rgt)

-}



---- Exercise 1 - define the reader monad -----------------------------------------

newtype Reader e a = Reader (e -> a)


-- help - meaning?
reader   :: (e -> a) -> Reader e a
reader f = Reader f

runReader                 :: Reader e a -> e -> a
runReader (Reader act) env = act env


ask :: Reader e e
ask = reader (\e -> e)


instance Monad (Reader e) where
    return x = reader (\_ -> x)  -- help why no arg in lambda?
    rd >>= k = reader $ \env ->
                            let x = runReader rd env
                                act' = k x
                            in runReader act' env

instance Functor (Reader e) where    -- help - don't know what this means, just posted it
    fmap = liftM                       -- for the code to work
instance Applicative (Reader e) where  -- help
    pure = return
    (<*>) = ap


type Env = Reader String
-- curried version of type Env a = Reader String a
-- help: how can you tell it is curried and not just a regular type without an 'a'?

test :: Env Int
test = do
    s <- ask --help what does this do?
    return $ read s + 1


--main = print $ runReader test "13"
-- help why does it need runReader if test returns 14 already?




--- Exercise 2 ----------------------------------------------------------------

-- use state monad from Control.Monad.State to re=implement the evaluator


data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
    deriving Show

type SymTab = M.Map String Double

type Evaluator a = State SymTab a

lookUp :: String -> Evaluator Double
lookUp str = do
    symTab <- get
    case M.lookup str symTab of
        Just v  -> return v
        Nothing -> error $ "Undefined variable " ++ str


addSymbol :: String -> Double -> Evaluator ()
addSymbol str val = do
    symTab <- get
    put $ M.insert str val symTab
    return ()


evaluate :: Tree -> Evaluator Double

evaluate (SumNode op left right) = do
    lft <- evaluate left
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
    return v


expr = AssignNode "x" (ProdNode Times (VarNode "pi")
                                (ProdNode Times (NumNode 4) (NumNode 6)))

main = print $ runState (evaluate expr) (M.fromList [("pi", pi)])
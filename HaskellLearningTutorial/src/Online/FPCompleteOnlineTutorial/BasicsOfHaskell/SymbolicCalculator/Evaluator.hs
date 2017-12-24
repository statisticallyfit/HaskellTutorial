{-# START_FILE Evaluator.hs #-}
module Evaluator (evaluate, Evaluator (..)) where

import Control.Monad (liftM, ap)
import Control.Applicative

import Lexer
import Parser
import qualified Data.Map as M



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



lookUp :: String -> Evaluator Double
lookUp str = Ev $ \symTab ->
    case M.lookup str symTab of
      Just v  -> (v, symTab)
      Nothing -> error $ "Undefined variable " ++ str

addSymbol :: String -> Double -> Evaluator Double
addSymbol str val = Ev $ \symTab ->
    let symTab' = M.insert str val symTab
    in  (val, symTab')



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
module Online.FPCompleteOnlineTutorial.BasicsOfHaskell.SymbolicCalculator.Main where

import qualified Data.Map as M
import Lexer (tokenize)
import Parser (parse)
import Evaluator

main = do
   loop (M.fromList [("pi", pi)])

loop symTab = do
   putStr "symcalcexpr> "
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
              -- help: why does it quit execution after all these changes?
              -- expecting it to continue and offer new chances...

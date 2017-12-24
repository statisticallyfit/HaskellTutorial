-- Redefine and using conditionals

import Prelude hiding (and)

and1 :: Bool -> Bool -> Bool
and1 a b = if a then
            if b then True else False
          else False

and2 :: Bool -> Bool -> Bool
and2 a b = if a then b else False

and3 :: Bool -> Bool -> Bool
and3 a b = if b then a else False

main = do
       print ([and1 True True, and1 True False, and1 False True,
               and1 False False])
       print ([and2 True True, and2 True False, and2 False True,
               and2 False False])
       print ([and3 True True, and3 True False, and3 False True,
               and3 False False])
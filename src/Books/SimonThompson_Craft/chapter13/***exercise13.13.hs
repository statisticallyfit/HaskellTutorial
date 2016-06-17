{-
HELP HELP
The first argument of `Info' should have kind `*',
      but `Show a' has kind `GHC.Prim.Constraint'

-}

instance  Info (Show a) => Show (a -> a)  where
  show function =  show (function (examples !! 0))
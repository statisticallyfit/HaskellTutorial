module TreeOps where



data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq, Show)




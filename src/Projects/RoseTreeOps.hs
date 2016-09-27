module RoseTreeOps where



data RoseTree a = Petal a | Briar a [RoseTree a] deriving (Eq, Show)


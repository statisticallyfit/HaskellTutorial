import Prelude hiding ((<), (>))


data Move = Rock | Paper | Scissors deriving (Eq, Show)


class NotEnumerated a where
    (<) :: a -> a -> Bool
    (<) _ _ = False

    (>) :: a -> a -> Bool
    (>) _ _ = False


instance NotEnumerated Move where


{-
NOTE testing:

*Main> Rock < Rock
False
*Main> Rock < Paper
False

-}
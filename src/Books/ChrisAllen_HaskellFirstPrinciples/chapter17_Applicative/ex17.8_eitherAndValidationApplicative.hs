import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



{- NOTE
-- f ~ Either e
(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: Either e (a -> b) -> Either e a -> Either e b
pure :: a -> f a
pure :: a -> Either e a
-}


-- data Either e a = Left e | Right a
data Sum a b = First a | Second b deriving (Eq, Show) -- to implement like Either
data Validation e a = Error e | Success a deriving (Eq, Show)
-- to implement by combining errors, unlike Either.

data Errors = DividedByZero
            | StackOverflow
            | MooglesChewedWires
            deriving (Eq, Show)


validToEither :: Validation e a -> Either e a
validToEither (Error e) = Left e
validToEither (Success a) = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left e) = Error e
eitherToValid (Right a ) = Success a

-- eitherToValid . validToEither == id
-- validToEither . eitherToValid == id



instance Functor (Sum a) where
    fmap = undefined

instance Applicative (Sum a) where
    pure = undefined
    (<*>) = undefined

-- same as Sum/Either
instance Functor (Validation a) where
    fmap = undefined

-- different
instance Monoid e => Applicative (Validation e) where
    pure = undefined
    (<*>) = undefined




{-
Your hint for this one is that you’re writing the following functions:

applyIfBothSecond :: (Sum e) (a -> b)
    -> (Sum e) a
    -> (Sum e) b
applyMappendError :: Monoid e => (Validation e) (a -> b)
    -> (Validation e) a
    -> (Validation e) b
-}



-- test data
t1 = Success (+2) <*> Success 1 -- Success 2
t2 = Success (+1) <*> Error [StackOverflow] -- Error [Stackoverflow]
t3 = Error [StackOverflow] <*> Success (+1) -- Error [StackOverflow]
t4 = Error [MooglesChewedWires] <*> Error [StackOverflow]
-- Error [MooglesChewedWires, StackOverflow]
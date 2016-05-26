import Data.Monoid hiding (Sum)
import Test.QuickCheck hiding (Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



{- NOTE
-- f ~ Either e
(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: Either e (a -> b) -> Either e a -> Either e b
pure :: a -> f a
pure :: a -> Either e a
-}

validToEither :: Validation e a -> Either' e a
validToEither (Error e) = Left' e
validToEither (Success a) = Right' a

eitherToValid :: Either' e a -> Validation e a
eitherToValid (Left' e) = Error e
eitherToValid (Right' a ) = Success a

-- eitherToValid . validToEither == id
-- validToEither . eitherToValid == id


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
data Either' e a = Left' e | Right' a deriving (Eq, Show)

instance Functor (Either' a) where
    fmap _ (Left' e) = Left' e
    fmap f (Right' a) = Right' (f a)

instance Applicative (Either' a) where
    pure = Right'
    (<*>) (Left' e) _ = Left' e
    (<*>) _ (Left' e) = Left' e
    (<*>) (Right' f) (Right' a) = Right' (f a)

------------------------------------------------------------------------------
instance (Arbitrary e, Arbitrary a) => Arbitrary (Either' e a)  where
    arbitrary = frequency [(1, Left' <$> arbitrary),
                           (3, Right' <$> arbitrary)]

instance (Eq e, Eq a) => EqProp (Either' e a) where
    (=-=) = eq



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
data Validation e a = Error e | Success a deriving (Eq, Show)
data Errors = DividedByZero
            | StackOverflow
            | MooglesChewedWires
            deriving (Eq, Show)


-- same as Sum/Either
instance Functor (Validation e) where
    fmap _ (Error e) = Error e
    fmap f (Success a) = Success (f a)

-- Different than Sum/Either
instance Monoid e => Applicative (Validation e) where
    pure = Success
    -- HELP HELP HELP why don't you need a Monoid for Errors?
    (<*>) (Error e1) (Error e2) = Error (e1 <> e2) -- <==
    (<*>) _ (Error e) = Error e
    (<*>) (Error e) _ = Error e
    (<*>) (Success f) (Success a) = Success (f a)
-- HELP why don't you have to declare Monoid e, e1, e2 in the top?

-------------------------------------------------------------------------------
instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = frequency [(1, Error <$> arbitrary),
                           (3, Success <$> arbitrary)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq
-------------------------------------------------------------------------------


{-
Your hint for this one is that you’re writing the following functions:
(FOR APPLICATIVE INSTANCES)

applyIfBothSecond :: (Sum e) (a -> b)
    -> (Sum e) a
    -> (Sum e) b
applyMappendError :: Monoid e => (Validation e) (a -> b)
    -> (Validation e) a
    -> (Validation e) b
-}


-- test data
{-t1 = (Success (+2)) <*> (Success 1) -- Success 2
t2 = (Success (+1)) <*> (Error [StackOverflow]) -- Error [Stackoverflow]
t3 = (Error [StackOverflow]) <*> (Success (+1)) -- Error [StackOverflow]
t4 = (Error [MooglesChewedWires]) <*> (Error [StackOverflow])-}
-- Error [MooglesChewedWires, StackOverflow]




main = do
-- HELP where do these type arguments go in the data declarations?
    quickBatch $ applicative (undefined :: (Either' String (Int,Int,Int)))
    quickBatch $ applicative (undefined :: (Validation String (String,Int,Int)))
    --print $ (Success (+2)) <*> (Success 1) -- HELP why error?
    --print t2
    --print t3
    --print t4
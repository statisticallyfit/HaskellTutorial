import Data.Monoid
import Test.QuickCheck
{-
INTERMISSION: EXERCISE:

Write a Monoid instance for Maybe (meaning optional) type which doesn’t require a
Monoid for the contents. Reuse the Monoid law QuickCheck properties
and use them to validate the instance.
-}
-- help: todo what does it mean - doesn't require a monoid for the contents?


monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


data Optional a = Nada | Only a
    deriving (Eq, Show)


newtype First' a = First' {
    getFirst' :: Optional a
} deriving (Eq, Show)

{-
instance Arbitrary Bull where
    arbitrary = frequency [(1, return Fools), (1, return Twoo)]
-}

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        x <- arbitrary  -- help understand what this means - is Arbitrary a Monad?
        frequency [(1, return (First' (Only x))),
                   (1, return (First' Nada))]


instance Monoid (First' a) where
    mempty = First' Nada
    mappend (First' (Only x)) _ = (First' (Only x))
    mappend (First' Nada) (First' (Only x)) = (First' (Only x))
    mappend _ _ = First' Nada


firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool


main :: IO()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: First' String -> Bool)
    quickCheck (monoidRightIdentity :: First' String -> Bool)
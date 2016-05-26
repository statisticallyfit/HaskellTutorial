import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{- NOTE
[1, 2, 3] <> [4, 5, 6]
-- changes to
[
1 <> 4
, 2 <> 5
, 3 <> 6
]
-}

-- ZERO vs IDENTITY ------------------------------------------------------------------
-- HELP how to get identity for ZipList?
-- Sum 1 `mappend` ??? -> Sum 1



zipListWith :: (a -> b -> c) -> List a -> List b -> List c
zipListWith _ Nil _ = Nil
zipListWith _ _ Nil = Nil
zipListWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipListWith f xs ys)

repeatList :: a -> (List a)
repeatList x = xs where xs = Cons x xs

take' :: Int -> List a -> List a
take' n Nil = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)


data List a = Nil | Cons a (List a) deriving (Eq, Show)
newtype ZipList a = ZipList (List a) deriving (Eq, Show)

instance Monoid (List a) where
    mempty = Nil
    mappend x Nil = x
    mappend Nil x = x
    mappend (Cons x xs) ys = Cons x $ xs `mappend` ys

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil -- HELP why is this the same as pure = const Nil ?
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (Cons f fs) <*> xs = fmap f xs <> (fs <*> xs)


instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (fmap f xs)

-- HELP HELP HELP figure out how to implement without helper methods.
instance Applicative ZipList where
    pure x = ZipList (repeatList x)
    (ZipList fs) <*> (ZipList xs) = ZipList (zipListWith id fs xs)



instance Arbitrary a => Arbitrary (List a) where
    arbitrary = frequency [(1, return Nil),
                           (4, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (ZipList a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList l) = xs -- help meaning?
                    in take' 3000 l
              ys' = let (ZipList l) = ys -- help meaning?
                    in take' 3000 l



-- tests
zf1 = ZipList $ Cons (+1) (Cons (+2) (Cons (+3) Nil))
zv1 = ZipList $ Cons   1  (Cons   2  (Cons   3  (Cons 4 Nil)))


zf2 = ZipList $ Cons (*8) (Cons (+1) Nil)
zv2 = ZipList $ Cons   4  (Cons   5  Nil)

main = do
    print $ zf1 <*> zv1
    print $ zf2 <*> zv2
    quickBatch $ applicative (ZipList (Cons ("b", "w", 1) Nil)) :: (ZipList (Int, Int, List Int))
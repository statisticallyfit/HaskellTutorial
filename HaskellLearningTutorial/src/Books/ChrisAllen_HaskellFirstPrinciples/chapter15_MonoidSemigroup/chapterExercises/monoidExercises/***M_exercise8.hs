import Data.Semigroup (Semigroup)
import Data.Monoid (Monoid, (<>) )
import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)

{-
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a
-}


-- NOTE type is:
-- runMem :: Mem s a -> s -> (a, s)  -- HELP meaning? in main it only takes 2 args...
newtype Mem s a = Mem { runMem :: s -> (a, s)}

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)                    -- HELP meaning?
    mappend Mem {runMem=f} Mem {runMem=g} = Mem $ \x -> let (a, b) = g x
                                                            (c, d) = f b
                                                        in (a <> c, d)
                                     -- help why doesn't this work with where
                                     -- and why does it work instead with let/in?

f = Mem $ \s -> ("hi", s + 1)


main = do
    print $ runMem (f <> mempty ) 0
    print $ runMem (mempty <> f) 0
    print $ (runMem mempty 0 :: (String, Int))
    print $ runMem (f <> mempty) 0 == runMem f 0
    print $ runMem (mempty <> f) 0 == runMem f 0

    -- HELP why doesn't work to put these in GHCI?
-- (,) a

t1 = (pure :: a -> (a,a)) 4
t2 = ((<*>) :: (,,) (a -> b -> c) -> (,) a -> (,) b -> (,) c) ("",(+2)) ("",2)


main = do
    print t1
    print t2




{-
NOTE
-- HELP what does a tuple of mempty mean?

instance (Monoid a, Monoid b) => Monoid (a, b) where
    mempty = (mempty, mempty)
    (a, b) 'mappend' (a', b') = (a 'mappend' a', b 'mappend b')


HELP what does the tuple in the first part of the tuple mean?

instance Monoid a => Applicative ((,) a) where
    pure x = (mempty, x)
    (u, f) <*> (v, x) = (u `mappend` v, f x)
-}

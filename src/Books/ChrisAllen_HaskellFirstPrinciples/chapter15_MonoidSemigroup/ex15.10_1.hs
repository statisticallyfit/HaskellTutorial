import Data.Monoid


-- Exercise: Write the monoid instance for our Maybe type renamed to Optional

data Optional a = Nada | Only a
    deriving (Eq, Show)


instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend Nada (Only a) = Only a
    mappend Nada Nada = Nada
    mappend (Only a) Nada = Only a
    mappend (Only a) (Only b) = Only (mappend a b)


main = do
    print $ Only (Sum 1) <> Only (Sum 1)
    print $ Only (Product 4) <> Only (Product 2)
    print $ Only (Sum 1) <> Nada
    print $ Only [1] <> Nada
    print $ Nada <> Only (Sum 1)


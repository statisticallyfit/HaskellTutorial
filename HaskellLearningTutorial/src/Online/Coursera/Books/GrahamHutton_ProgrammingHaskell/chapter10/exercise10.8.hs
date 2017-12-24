

data Maybe' a = Nothing' | Just' a deriving (Eq, Ord, Show)


instance Functor Maybe' where
    fmap _ Nothing'  = Nothing'
    fmap f (Just' a) = Just' (f a)

instance Applicative Maybe' where
    pure                      = Just'
    (<*>) _ Nothing'          = Nothing'
    (<*>) Nothing' _          = Nothing'
    (<*>) (Just' f) (Just' a) = Just' (f a)

instance Monad Maybe' where
    return            = pure
    (>>=) Nothing' _  = Nothing'
    (>>=) (Just' a) f = f a

-- note:
-- (>>=) :: m a -> (a -> m b) -> m b

tf1 = fmap (+1) Nothing'
tf2 = fmap (+1) (Just' 3)
ta1 = (Just' (+1)) <*> (Just' 1)
ta2 = pure 10 :: Maybe' Int
ta3 = pure "hi there" :: Maybe' String
tm1 = return 10 :: Maybe' Int
tm2 = (Just' 1) >>= return . (+1) -- ) :: Maybe' Int)


main = do
    print tf1
    print tf2
    print ta1
    print ta2
    print ta3
    print tm1
    print tm2
    print tm2




    {-
    instance Monad [] where
        return :: a → [ a ]
        return x =[x ]
        (>>=) :: [ a ] → (a → [ b ]) → [ b ]
        xs >>= f = concat (map f xs)

    -- HELP how to test?
    -}
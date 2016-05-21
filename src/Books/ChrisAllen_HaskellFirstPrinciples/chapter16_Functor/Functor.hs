import Data.Either
{-
NOTE
class Functor f where
    fmap :: (a -> b) -> f a -> f b   note: has kind * -> * -> * (query :k (->) in ghci)

note: argument f is a Functor instance that takes a type argument a.

NOTE the type of fmap specializes to different types
NOTE: HELP type e is ignored in the tuple and Either and Constant exampl.es.
-- Functor f =>
fmap :: (a -> b) -> f a -> f b
:: (a -> b) -> [ ] a -> [ ] b
:: (a -> b) -> Maybe a -> Maybe b
:: (a -> b) -> Either e a -> Either e b
:: (a -> b) -> (e,) a -> (e,) b
:: (a -> b) -> Identity a -> Identity b
:: (a -> b) -> Constant e a -> Constant e b

-}

{-
note:
* map and fmap both apply a function over a structure
* but map cannot enter into the structure sometimes, while fmap can:
-}

{-
HELP getting error
instance (Show a, Show b) => Show (Either a b) where
    show (Left x) = "Left " ++ show x -- note: x is showable due to precondition in type above.
    show (Right y) = "Right " ++ show y
-}

{- uncover
main = do
    print $ map (\x -> x > 3) [1..6]; print $ fmap (\x -> x > 3) [1..6]
    --print $ map (+1) (Just 1) -- gives error
    print $ fmap (+1) (Just 1)  -- key: fmap enters inside Maybe
    print $ fmap (10/) (4,5)    -- key: fmap enters inside tuple
    --print $ fmap (++ ", Esq.") (Right "Chris Allen") -- key: fmap enters inside Either
    -- HELP why does this give error?
-}




-- 16.4 ------------------------------------------------------------------------------

data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
    fmap :: Functor f => (a -> b) ->   f a ->   f b
    -- note meaning:         f       (Pls a) = Pls (f a)
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)

    {-
        While f is used in the type of fmap to represent the Functor, by convention,
        it is also conventionally used in function deﬁnitions to name
        an argument that is itself a function. Don’t let the names fool you
        into thinking the f in our FixMePls instance is the same f as in the
        Functor typeclass deﬁnition. note: but it represents the same function.
    -}
main = do
    print $ fmap (+1) (Pls 1)
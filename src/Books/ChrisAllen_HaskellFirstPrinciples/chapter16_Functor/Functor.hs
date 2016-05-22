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


{-
HELP don't understand this error
The first argument of `Functor' should have kind `* -> *',
      but `FixMePls a' has kind `*'
    In the instance declaration for `Functor (FixMePls a)'

    when type is: instance Functor (FixMePls a) where
-}


instance Functor FixMePls where
    --fmap :: Functor f => (a -> b) ->   f a ->   f b -- HELP why error when uncommented?
    -- note meaning:         f       (Pls a) = Pls (f a)
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)

    {- HELP - f a is not Pls a since f is (a -> b) so help
        While f is used in the type of fmap to represent the Functor, by convention,
        it is also conventionally used in function deﬁnitions to name
        an argument that is itself a function. Don’t let the names fool you
        into thinking the f in our FixMePls instance is the same f as in the
        Functor typeclass deﬁnition. note: but it represents the same function.
    -}
{-uncover
main = do
    print $ fmap (+1) (Pls 1)
-}








-- 16.5 FUNCTOR LAWS ----------------------------------------------------------------

-- NOTE: the function (f) must be any function with a type that is an instance
-- of Functor


-- note: first law: identity
-- fmap id anything = anything

-- note: second law: composition:
-- fmap (f . g)  == fmap f . fmap g

{-uncover
main = do --------------------------------- identity law
    print $ fmap id "Hi Julie"
    print $ fmap id (Just 1)
    --------------------------------------- composition law
    print $ fmap ((+1) . (*2)) [1..5]
    print $ fmap (+1) . fmap (*2) $ [1..5]
-}







-- 16.6 (TESTING FUNCTOR LAWS) ----------------------------------------------------

data WhoCares a = ItDoesnt
                | Matter a
                | WhatThisIsCalled
                deriving (Eq, Show)


-- NOTE this is a law abiding Functor instance (identity)
instance Functor WhoCares where
    fmap _ ItDoesnt = ItDoesnt
    fmap _ WhatThisIsCalled = WhatThisIsCalled
    fmap f (Matter a) = Matter (f a)



-- NOTE THIS IS NOT law abiding functor instance (identity)
{-
instance Functor WhoCares where
    fmap _ ItDoesnt = WhatThisIsCalled
    fmap f WhatThisIsCalled = ItDoesnt
    fmap f (Matter a) = Matter (f a)
-}

{-uncover HELP why doesn't this work? Need to do it directly in ghci
main = do
    print $ fmap id ItDoesnt
    print $ fmap id WhatThisIsCalled
    print $ fmap (+10) (Matter 10)
-}







data CountingBad a = Heisenberg Int a deriving (Eq, Show)

-- NOTE this is NOT law abiding (composition)
instance Functor CountingBad where
    fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)


-- NOTE this IS law abiding (composition)




oneWhoKnocks = Heisenberg 0 "Uncle"
f = (++ " Jesse")
g = (++ " lol")

main = do
    print $ fmap (f . g) oneWhoKnocks
    print $ fmap f . fmap g $ oneWhoKnocks
import Test.QuickCheck
import Test.QuickCheck.Function -- for Fun type in functorCompose'

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







data CountingBad a = HeisenbergB Int a deriving (Eq, Show)
data CountingGood a = HeisenbergG Int a deriving (Eq, Show)

-- NOTE this is NOT law abiding (composition)
instance Functor CountingBad where
    fmap f (HeisenbergB n a) = HeisenbergB (n+1) (f a)


-- NOTE this IS law abiding (composition)
instance Functor CountingGood where
    fmap f (HeisenbergG n a) = HeisenbergG (n) (f a) -- NOTE: just leave alone what is
    -- not the final type argument in our f in Functor.


oneWhoKnocks = HeisenbergG 0 "Uncle"  -- doesn't matter which heisenberg is used.
f = (++ " Jesse")
g = (++ " lol")


{-uncover
main = do
    print $ fmap (f . g) oneWhoKnocks
    print $ fmap f . fmap g $ oneWhoKnocks -- note: see? supposed to be the same.
    ------------------------------------------
-}









-- 16.7 COMMONLY USED FUNCTORS

replaceWithP = const 'p'
-- note: const :: a -> b -> a so it takes another argument to yield an 'a'.

tossEmOne = fmap (+1) negate

{-uncover
main = do
    -- data Maybe a = Nothing | Just a
    print $ fmap replaceWithP (Just 10)
    print $ fmap replaceWithP Nothing
    -- data [] a = [] | a : [a]     help what does this mean?
    print $ fmap replaceWithP [1,2,3,4,5]
    print $ fmap replaceWithP "Ave"
    print $ fmap (+1) []
    print $ fmap replaceWithP []
    -- data (,) a b = (,) a b    note the tuple
    print $ fmap replaceWithP (10, 20) -- help why does it only do the last component?
    print $ fmap replaceWithP (10, "woo")
    -- now the instance for functions
    print $ tossEmOne 10
    print $ tossEmOne (-10)
-}




-- NOTE: functors are stacked!
lms = [Just "Ave", Nothing, Just "woohoo"]  -- type equals List (Maybe (String))
--replaceWithP = const 'p'

{-uncover
main = do
    -- equals List (Maybe (String)) -> Char
    print $ replaceWithP lms
    -- equals List ( FMAPARG (Maybe String)) -> List Char
    print $ fmap replaceWithP lms
    -- equals List ( Maybe (FMAPARG String)) -> List (Maybe Char)
    -- equals List ( Maybe (FMAPARG (List Char))) -> List (Maybe Char)
    print $ (fmap . fmap) replaceWithP lms
    -- equals List (Maybe ( List (FMAP ARG Char))) -> List (Maybe List (Char))
    print $ (fmap . fmap . fmap) replaceWithP lms
-}




--lmls ~ List (Maybe (List String))
ha = Just ["Ha", "Ha"]
lmls = [ha, Nothing, Just[]]
{-uncover
main = do
    print $ fmap replaceWithP lmls
    print $ (fmap . fmap) replaceWithP lmls
    print $ (fmap . fmap . fmap) replaceWithP lmls
    print $ (fmap . fmap . fmap . fmap) replaceWithP lmls
-}






-- 16.8 Mapping over structure to transform the unapplied type arg ------------

{-
NOTE when you include more args, you reduce kindedness of the type.
So that is why we include args for Two and Or in Functor instances, to
fit the Functor * -> * kind
HELP what does it mean that functor has a kind? Where does this fit?

Prelude> :k Either
Either :: * -> * -> *
Prelude> :k Either Integer
Either Integer :: * -> *
Prelude> :k Either Integer String
Either Integer String :: *
-}


data Two a b = Two a b deriving (Eq, Show)

data Or a b = First a | Second b deriving (Eq, Show)


-- note we must leave the argument 'a' alone now that it has been put in
-- the type of fmap
{-
class Functor f where
    fmap :: Functor => (a -> b) -> f a -> f b

    so here f ~ (Two a)    note so we can't touch 'a' now.

    key can also have b in the title, letter doesn't matter
    instance Functor (Two b) where
        fmap f (Two a b) = Two a (f b)

    key letter doesn't matter
    instance Functor (Or b) where
        fmap _ (First a) = First a
        fmap f (Second b) = Second (f b)
-}
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)











-- 16.9 QuickCheck Functor instances


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)


functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)


li x = functorCompose (+1) (*2) (x :: [Int])


type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

{- NOTE (pag 629 pdf)
There are a couple things going on here. One is that we needed
to import a new module from QuickCheck. Another is that we’re
pattern matching on the Fun value that we’re asking QuickCheck to
generate. The underlying Fun type is essentially a product of the
weird function type and an ordinary Haskell function generated from
the weirdo. The weirdo QuickCheck-speciﬁc concrete function is
a function represented by a datatype which can be inspected and
recursed. We only want the second part, the ordinary Haskell function,
so we’re pattern-matching that one out. HELP what pattern matching out?
-}

{-uncover
main = do
    quickCheck $ \x -> functorIdentity (x :: [Int])
    quickCheck li
    quickCheck (functorCompose' :: IntFC) -- HELP why  f a equals [Int]???
-}
















-- 16.11 MAYBE and EITHER FUNCTORS -----------------------------------------------

-- MAYBE -------------------------------------------------------------------------

incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing



-- more cleaner versions
incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe s = fmap show s


-- more abstract
incMaybe' :: Num a => Maybe a -> Maybe a
incMaybe' = fmap (+1)

showMaybe' :: Show a => Maybe a -> Maybe String
showMaybe' = fmap show


-- lifted versions
-- figured out by typing: :t fmap (+1) gives (Functor f, Num b) => ....
liftedIncM :: (Functor f, Num b) => f b -> f b
liftedIncM = fmap (+1)

liftedShowM :: (Functor f, Show a) => f a -> f String
liftedShowM = fmap show


{-uncover
main = do
    print $ incMaybe (Just 1); print $ incIfJust (Just 1)
    print $ showMaybe (Just 9001); print $ showIfJust (Just 9001)
    --print $ showMaybe Nothing  -- HELP why error when prints??
    --fmap incMaybe Nothing

    --print $ liftedIncM (Just 1) -- HELP why error?
    --print $ liftedIncM Nothing
    --print $ liftedShowM (Just 1)
    --print $ liftedShowM Nothing
    -- liftedIncM [1..5]
-}






-- EITHER -------------------------------------------------------------------------

-- HELP what is the 'e' and 'a' mean? Is 'n' the 'a'?
incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left e) = Left e

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Right s) = Right $ show s
showIfRight (Left e) = Left e



-- clearer
incEither :: Num a => Either e a -> Either e a
incEither m = fmap (+1) m

showEither :: Show a => Either e a -> Either e String
showEither s = fmap show s


-- eta contract
incEither' :: Num a => Either e a -> Either e a
incEither' = fmap (+1)

showEither' :: Show a => Either e a -> Either e String
showEither' = fmap show


-- f ~ Either e
liftedIncE :: (Functor f, Num b) => f b -> f b
liftedIncE = fmap (+1)

liftedShowE :: (Functor f, Show a) => f a -> f String
liftedShowE = fmap show


--uncover
--main = do
    -- HELP how to print without errors?
    --(fmap . print) (incEither (Right 1)) --; print $ incEither (Left 1)
















-- 16.12 SURPRISING FUNCTOR -------------------------------------------------------

{-
note
The constant data type has a phanton type (b), meaning it has no corresponding
witness at the value/term level. But it does exist since
:k Constant
Constant :: * -> * -> *
-}
newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Show)


instance Functor (Constant a) where
    fmap _ (Constant b) = Constant b

{-
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool
-}
{--uncover
main = do
    print $ Constant 2
    putStr "#1: "; print $ (const 2) (Constant 3)
    putStr "#2: "; print $ const 2 (getConstant (Constant 3))
    putStr "#3: "; print $ fmap (const 2) (Constant 3)
    putStr "#3a "; print $ fmap (const "hi") (Just 4)
    putStr "#4: "; print $ getConstant $ fmap (const 2) (Constant 3)
    putStr "#5: "; print $ getConstant $ fmap (const "blah") (Constant 3)
-}
-- HELP why does this work the way it does?
-- understand better:
{-
When you fmap the const function over the Constant type, the ﬁrst
argument to const is never used because the partially applied const is
itself never used. The ﬁrst type argument to Constant’s type constructor
is in the part of the structure that Functor skips over. The second
argument to the Constant type constructor is the phantom type variable
b which has no value or term-level witness in the datatype. Since
there are no values of the type the Functor is supposed to be mapping,
we have nothing we’re allowed to apply the fmap’d function to, so we
never use the const expressions.
-}

-- NOTE: checking if Constant adheres to Functor laws
{-
separate = fmap (const 3) . fmap (const 5)
fused = fmap ((const 3) . (const 5))

main = do
    -- identity
    print $ getConstant (id (Constant 3))  -- HELP why do both work? How?
    print $ getConstant (fmap id (Constant 3))
    -- composition
    print $ ((const 3) . (const 5)) 10
    print $ ((const 5) . (const 3)) 10
    print $ getConstant $ separate $ (Constant "WOOHOO")
    print $ getConstant $ fused $ (Constant "WOOHOO")
-}










-- 16.13 MORE STRUCTURE MORE FUNCTORS ---------------------------------------------

data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (fmap f fa)
    -- HELP understand better
{-uncover
main = do
    print $ fmap (+1) (Wrap (Just 1))
    print $ fmap (+1) (Wrap [1,2,3])
-}








-- 16.14 IO FUNCTOR ---------------------------------------------------------------

--getLine :: IO String
-- read :: Read a => String -> a

getInt :: IO Int
getInt = fmap read getLine -- note: fmap lifts read over the IO type

{-uncover
main = do
    --print $ fmap (const ()) getInt            -- HELP understand better
    print $ (const ()) getInt                   -- note: prints the () from const
    --print $ fmap (+1) getInt                  -- HELP errors again
    --print $ fmap (++ " and me too!") getLine
-}




meTooIsm :: IO String
meTooIsm = do
    input <- getLine
    return (input ++ " and me too!")

bumpIt :: IO Int
bumpIt = do
    intVal <- getInt
    return (intVal + 1)

{-uncover
main = do
    meTooIsm
    putStrLn ""
    --bumpIt -- help why doesn't this work to have them both in the same main?
-}












-- 16.15 NATURAL TRANSFORMATIONS -------------------------------------------------
-- note means leaving contents unchanged and changing the structure

type Nat f g = forall a . f a -> g a -- like opposite of functor

-- NOTE: in prelude ghci, do :
-- :set -XRank2Types  so the above notation works

-- This'll work
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]
-- This will not work, not allowed.
degenerateMtl :: Nat Maybe []
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a+1]




-- Bad because a is in the type
type BadNat f g a = f a -> g a

-- This'll work
maybeToList' :: BadNat Maybe [] a
maybeToList' Nothing = []
maybeToList' (Just a) = [a]
-- But this will too if we tell it
-- 'a' is Num a => a
degenerateMtl' :: Num a => BadNat Maybe [] a
degenerateMtl' Nothing = []
degenerateMtl' (Just a) = [a+1]

-- note: bad that it works since we're only supposed to change the structure
-- and not the internal content!












-- 16.16 UNIQUE FUNCTORS ---------------------------------------------------------
-- note functors are unique for a given data type
 -- page 642 pdf HELP understand this section better

data Tuple a b = Tuple a b deriving (Eq, Show)


-- note impossible in haskell
{-
instance Functor (Tuple ? b) where
    fmap f (Tuple a b) = Tuple (f a) b
-}

-- note two ways to address this - 1) flip args to type constructor, 2) make new
-- datatype using Flip newtype


newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

{- note
:k (Flip Tuple)
(Flip Tuple) :: * -> * -> *
so take out the arg 'a' :
-}
-- HELP says illegal declaration
--instance Functor (Flip Tuple a) where
--    fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b


--main = print $ fmap (+1) (Flip (Tuple 1 "blah"))







import Data.Monoid -- need for Sum and Product to work
import Test.QuickCheck
import Control.Monad

-- 15.3 MONOID

{-
-- Monoid is a function that takes two args and follows two laws:
-- associativity and identity.
-- mappend x mempty = x
-- mappend mempty x = x
-- identity function means the other is returned and other is ignored (like
-- multiplying by one or addding zero).

note: Pattern of monoid: types that have binary functions that let you join things
together using associativity and an identity value that will return the other
argument unmodified.
note: this pattern is used by summation, multiplication, list concatenation... etc.


Monoid typeclass is:

class Monoid m where
    mempty  :: m             equals the identity value for mappend operation.
    mappend :: m -> m -> m   equals how any two values of your type can be joined.
    mconcat :: [m] -> m
    mconcat = folder mappend mempty


mappend also can be written:

(<>) :: Monoid m => m -> m -> m


note: Monoid does not include law of commutativity

-}




-- 15.5 EXAMP.LES OF MONOID: --------------------------------------------------------

-- LIST

{-
note: the list instance for monoid
instance Monoid [a] where
    mempty = []
    mappend = (++)
-}
{- uncover
main = do
    print $ mappend [1,2,3] [4,5,6]
    print $ mconcat [[1..3], [4..6]]
    print $ mappend "Trout" " goes well with garlic"
-}




-- INTEGER
-- note: type integer has no monoid instance since it's not clear if the integers
-- should be added or subtracted. Typeclass instances are unique to the types
-- they are for.
{- uncover
main = do
    print $ mappend (Sum 1) (Sum 5)
    print $ mappend (Product 5) (Product 7)
    print $ mappend (Sum 4.5) (Sum 3.4)
    ----------------------------------------------------
    -- note: <> (which is mappend) only works if argument is a Num
    --print (Sum "Frank" <> Sum " " <> Sum "Herbert") -- note: No instance for (Num [Char]) arising from a use of `<>'
    print $ mappend (Sum 8) (Sum 9)
    print $ mappend mempty Sum 9
    -- note: mappend only takes 2 arguments so cannot add 3 or more numbers , BUT:
    print $ mappend (Sum 1) (mappend (Sum 2) (Sum 3))
    print $ (Sum 1) <> (Sum 1) <> (Sum 1)
    print $ (Sum 1) `mappend` (Sum 1) `mappend` (Sum 1)
    print $ mconcat [(Sum 8), (Sum 9), (Sum 10)]
    print $ getSum $ mappend (Sum 1) (Sum 1)
    print $ getProduct $ mappend (Product 5) (Product 5)
    print $ getSum $ mconcat [(Sum 5), (Sum 6), (Sum 7)]
-}


{-
note note note:
- integers form a monoid under summation
- integers form a monoid under multiplication
...
- lists form a monoid under concatenation (lists have more than one possible monoid)
-}




-- 15.8  MONOID LAWS:

{-
note left identity
mappend mempty x = x

note right identity
mappend x mempty = x

note associativity
mappend x (mappend y z) = mappend (mappend x y) z - help - are there other examples?

mconcat = foldr mappend mempty
-}

{-uncover
main = do ------------------------------------------------- Monoid Sum instance
    print $ mappend mempty (Sum 1) -- left identity
    print $ mappend (Sum 1) mempty -- right identity
    print $ (Sum 1) <> (Sum 2 <> Sum 3) -- associativity
    print $ (Sum 1 <> Sum 2) <> (Sum 3) -- associativity
    print $ mconcat [Sum 1, Sum 2, Sum 3] -- mconcat has same result as next line
    print $ foldr mappend mempty [Sum 1, Sum 2, Sum 3]
    ------------------------------------------------------- Monoid List instance
    print $ mappend mempty [1,2,3] -- left
    print $ mappend [1,2,3] mempty -- right
    print $ [1] <> ([2] <> [3]) -- associativity
    print $ ([1] <> [2]) <> [3] -- associativity
    print $ mconcat [[1], [2], [3]] -- mconcat ~ foldr mappend mempty
    print $ foldr mappend mempty [[1], [2], [3]]
-}




-- BOOL ----------------------------------------------------------------------------

-- note: bool has different instances under Monoid: All for && and Any for ||

-- Mappending is best thought of as way to condense values to a summar.y value
-- and NOT combining values as do addition/concatenation of lists.

{-uncover
main = do
    print $ All True <> All True
    print $ All True <> All False
    print $ All False <> All True
    print $ All False <> All False

    print $ Any True <> Any True
    print $ Any True <> Any False
    print $ Any False <> Any True
    print $ Any False <> Any False

-}





-- MAYBE ---------------------------------------------------------------------------

-- note: First returns leftmost nonNothing value
-- note: Last returns rightmost non-Nothing value

{-uncover
main = do
    print $ First (Just 1) `mappend` First (Just 2)
    print $ Last (Just 1) `mappend` Last (Just 2)
    print $ Last (Just 2) `mappend` Last Nothing
    print $ First Nothing `mappend` First (Just 2)
    -- help todo error when printing these need help
    --putStrLn $ First Nothing `mappend` First Nothing
    --print $ Last Nothing `mappend` Last Nothing
-}








--- 15.10 Reusing algebras by asking for algebras -----------------------------------

-- note (+) but not (++) is commutative

evilPlus = flip (+)
evilPlusPlus = flip (++)
{-uncover
main = do
    print $ 76 + 67
    print $ evilPlus 76 67
    print $ [1..3] ++ [4..6]
    print $ evilPlusPlus [1..3] [4..6]
-}





-- 15.12 Quickcheck

-- say type S = String and type B = Bool for brevity
type S = String
type B = Bool


-- note: creating abstract function to test associativity of functions
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

{-uncover
main = do
    quickCheck (monoidAssoc :: S -> S -> S -> B)
    --verboseCheck (monoidAssoc :: S -> S -> S -> B)
    -----------------------------------------------
    quickCheck (monoidLeftIdentity :: S -> B)
    quickCheck (monoidRightIdentity :: S -> B)
-}






-- proving why Bool's identity can't be false
data Bull = Fools | Twoo
    deriving (Eq, Show)


instance Arbitrary Bull where
    arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools
    -- note: fails test since this is defined as:
    {-
    mappend mempty x = Fools
    mappend x mempty = Fools     note: fools is not x so it fails identity laws.
    -}

type BullMappend = Bull -> Bull -> Bull -> Bool



{-uncover
main :: IO()
main = do
    quickCheck (monoidAssoc :: BullMappend)
    quickCheck (monoidLeftIdentity :: Bull -> Bool)
    quickCheck (monoidRightIdentity :: Bull -> Bool)
-}







-- 15.13 Semigroup -----------------------------------------------------------------

-- note: semigroup is like the superclass of Monoid - it is equal to monoid
-- just doesn't have the identity property, so just has the mappend property
-- that obeys associativity.
-- (<>) :: a -> a -> a
-- (a <> b) <> c = a <> (b <> c)


-- note: the NonEmpty list type has a Semigroup but not Monoid instance
{-
data NonEmpty a = a :| [a]
    deriving (Eq, Ord, Show)

-- note: the :| is an infix data constructor that takes two type arguments
-- which are 'a' and '[a]'. The :| is a product of 'a' and '[a]'.
-- Guarantees we will always have at least one value of type a , which [a] does not
-- guarantee as any list might be emtpy.
-- note: because :| data constructor is not alphanumeric, it must be infix.

-- note: because NonEmpty is a product of two arguments it could be written:

newtype NonEmpty a = NonEmpty (a, [a])
    deriving (Eq, Ord, Show)
-}
-- help: todo: understand why there is no identity value in design of NonEmpty.


-- you need to have `semigroups` installed
{-
Prelude> import Data.List.NonEmpty as N
Prelude N> import Data.Semigroup as S
Prelude N S> 1 :| [2, 3]
1 :| [2,3]
Prelude N S> :t 1 :| [2, 3]
1 :| [2, 3] :: Num a => NonEmpty a
Prelude N S> :t (<>)
(<>) :: Semigroup a => a -> a -> a
Prelude N S> let xs = 1 :| [2, 3]
Prelude N S> let ys = 4 :| [5, 6]
Prelude N S> xs <> ys
1 :| [2,3,4,5,6]
Prelude N S> N.head xs
1
Prelude N S> N.length (xs <> ys)
6
-}

main = print ""
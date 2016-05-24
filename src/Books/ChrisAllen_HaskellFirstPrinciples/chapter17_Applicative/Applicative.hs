import Control.Applicative
import Data.Monoid -- for Sum


{-
NOTE
Applicative is a monoidal functor. Applies a functor over structures by smashing
them together.

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

note
-- fmap
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b


note
Along with these core functions, the Control.Applicative library provides
some other convenient functions: liftA, liftA2, and liftA3:

liftA :: Applicative f =>
    (a -> b)
    -> f a
    -> f b
liftA2 :: Applicative f =>
    (a -> b -> c)
    -> f a
    -> f b
    -> f c
liftA3 :: Applicative f =>
    (a -> b -> c -> d)
    -> f a
    -> f b
    -> f c
    -> f d


    note liftA is basically fmap only with an Applicative typeclass constraint
    instead of a Functor one. But all Applicative instances are also functors.

-}




-- 17.3 FUNCTOR vs. APPLICATIVE -----------------------------------------------------

-- LAW: fmap f x = pure f <*> x
{-uncover
main = do
    print $ fmap (+1) [1,2,3]
    print $ pure (+1) <*> [1..3]
-}


{- NOTE
mappend :: Monoid a => a -> a -> a

So, with Applicative, we have a Monoid for our structure and function
application for our values!

mappend :: f f f
$ :: (a -> b) a b

(<*>) :: f (a -> b) -> f a -> f b
-- plus Functor fmap to be able to map
-- over the f to begin with.


So in a sense, we’re bolting a Monoid onto a Functor to be able to deal
with functions embedded in additional structure. In another sense,
we’re enriching function application with the very structure we were
previously merely mapping over with Functor.
-}

-- EXAMPLES
{-uncover
main = do
    -- note So here in f (a -> b) the list is (f). THe mappend of applicative
    -- prevents the final result from being a list of lists. It joins since it
    -- is a monoidal functor.
    print $ [(*2), (*3)] <*> [4,5]
    -- note The f is the Maybe. The join aspect prevents it from being a maybe
    -- of maybe.
    print $ (Just (*2)) <*> Just 2
    print $ Just (*2) <*> Nothing
    --print $ Nothing <*> Just 2  -- HELP error when printing!
    --print $ Nothing <*> Nothing -- HELP error when printing!
-}



{-
NOTE

Prelude> :info (,)
data (,) a b = (,) a b -- Defined in ‘GHC.Tuple’
...
instance Monoid a => Applicative ((,) a) -- Defined in ‘GHC.Base’
...
instance (Monoid a, Monoid b) => Monoid (a, b)

For two tuples, the 'a' part must be Monoid type since the 'a's in the two
corresponding tuples must be joined. Not needed for the 'b' part since the
functor is just applied over that. (help - the functor is applied or what?)
-}

-- Example
{-uncover
main = do
    print $ ("Woo", (+1)) <*> (" Hoo!", 0)
    print $ ((Sum 2), (+1)) <*> ((Sum 0), 0)
    print $ ((Product 3), (+9)) <*> ((Product 2), 8)
    print $ ((All True), (+1)) <*> ((All False), 0)
-}










{-
NOTE TUPLE MONOID AND APPLICATIVE --------------------------------------------------

-- HELP what does a tuple of mempty mean?

instance (Monoid a, Monoid b) => Monoid (a, b) where
    mempty = (mempty, mempty)
    (a, b) 'mappend' (a', b') = (a 'mappend' a', b 'mappend b')


HELP what does the tuple in the first part of the tuple mean?

instance Monoid a => Applicative ((,), a) where
    pure x = (mempty, x)
    (u, f) <*> (v, x) = (u `mappend` v, f x)






NOTE MAYBE MONOID and APPLICATIVE ------------------------------------------------

note Monoid has kind (*) so just like Functor, use the argument to reduce kind
of the type for which we are writing the instance.


instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    mappend m Nothing = m
    mappend Nothing m = m
    mappend (Just a) (Just a') = Just (mappend a


HELP Applicative has kind (* -> *) which is a function so how do we write
the type instance?
HELP why is there no monoid mention in the applicative?

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    _ <*> Nothing = Nothing
    Just f <*> Just a = Just (f a)


HELP understand this paragraph: pg 657:

While applicatives are really monoidal functors, be careful about
taking it too literally. For one thing, Monoid and Applicative instances
aren’t required or guaranteed to have the same monoid of structure,
and the functorial part may actually change things. Nevertheless,
you might be able to see the implicit monoid in how the Applicative
pattern matches on the Just and Nothing cases and compare that with
this Monoid:
-}










-- 17.5 APPLICATIVE IN USE -------------------------------------------------------


{-

note LIST APPLICATIVE -------------------------------------------------------------

-- f ~ []
(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: [ ] (a -> b) -> [ ] a -> [ ] b

-- more syntactically typical
(<*>) :: [(a -> b)] -> [a] -> [b]


pure :: a -> f a
pure :: a -> [ ] a


example list
[(+1), (*2)] <*> [2,4]
= [ (+1) 2 , (+1) 4 , (*2) 2 , (*2) 4 ]
= [2, 5, 4, 8]

so here the (a -> b) functions are encased in a list.

note
The fact that it doesn’t
return two lists or a nested list or some other conﬁguration in which
both structures are preserved is the monoidal part; the reason we
don’t have a list of functions merely concatenated with a list of values
is the function application part



example tuple
= (,) <$> [1,2] <*> [3,4]
 = [(1, ), (2, )] <*> [3,4]
 = [(1,3), (1,4), (2,3), (2,4)]

note could write instead
= liftA2 (,) [1,2] [3,4]
= [(1,3),(1,4),(2,3),(2,4)]




example
= (+) <$> [1,2] <*> [3,5]
= [4,6,5,7]

= liftAd (+) [1,2] [3,5]
= [4,6,5,7]




example
= max <$> [1,2] <*> [1,4]
= [1,4,2,4]

= liftA2 max [1,2] [1,4]
= [1,4,2,4]
-}
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


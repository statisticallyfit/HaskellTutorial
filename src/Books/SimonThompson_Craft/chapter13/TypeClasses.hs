
-- 13.3 SIGNATURES AND INSTANCES ----------------------------------------------------

{-
NOTE declaring a class

class Info a where
    exmples :: [a]
    size    :: a -> Int

NOTE to create an instance of a class with that type
instance Eq Bool where
    True  == True  = True
    False == False = True
    _ == _         = False
-}

class Info a where
    examples :: [a]
    size     :: a -> Int
    size _   = 1 -- NOTE example of overriding - now we have this definition
    -- in the class and not in toplevel (in each instance) so for each instance
    -- size function is overridden.

instance Info Char where
    examples = ['a', 'A', 'z', 'Z', '0', '9']
    --size _   = 1 -- note each char has size 1.

instance Info Bool where
    examples = [True, False]
    --size _   = 1

instance Info Int where
    examples = [-100 .. 100]
    --size _   = 1



data Shape = Circle Float | Rectangle Float Float deriving (Eq, Ord, Show)


area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle l w) = l * w


-- HELP how to write this if it were (Shape a)?
instance Info Shape  where
    examples = [Circle 3.0, Rectangle 45.9 87.6]
    --size     = round . area







-- Instances and contexts ----------

-- note key - defining examples and size over [a] type
instance Info a => Info [a] where
    -- note has type [[a]]
    examples = [ [] ] ++
               [ [x] | x <- examples ] ++
               [ [x,y] | x <- examples, y <- examples ]
    -- note has type [a]; we need context of Info a => above because we are
    -- using the size that takes an (a) in map size over [a]s .
    size     = foldr (+) 1 . map size







-- Derived Classes -------------------

{- NOTE
class Eq a => Ord a where  --- note ORd inherits from Eq.
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a
    c0mpare              :: a -> a -> Ordering
-}





-- Multiple Constraints ----------------

{-
NOTE example 1
vSort :: (Ord a, Show a) => [a] -> String
vSort = show . iSort

example 2
instance (Eq a, Eq b) => Eq (a, b) where
    (x,y) == (z,w) = x == z && y == w

example 3
class (Ord a, Show a) => OrdShow a -- note multiple inheritance HELP OrdShow?
-}



{-
NOTE - instead of defining many functions, we use Checkable class ...

infoCheck :: (Info a) => (a -> Bool) -> Bool
infoCheck property = and (map property examples)

infoCheck2 :: (Info a, Info b) => (b -> a -> Bool) -> Bool
infoCheck2 property = and (map (infoCheck . property) examples)

-}

-- NOTE says that type is Checkable if something can be checked by applying it to
-- the . given in an Info type
class Checkable b where
    infoCheck :: (Info a) => (a -> b) -> Bool

-- note definition of infoCheck.1 is same as instance for Bool)
instance Checkable Bool where
    infoCheck property = and (map property examples)

-- note for infoCheck2
-- HELP meaning? what does Checkable (a -> b) mean and why is the type of
-- infoCheck2 (b -> a -> Bool)? Why need b and a? Why need a Info and b Checkable?
instance (Info a, Checkable b) => Checkable (a -> b) where
    infoCheck property = and (map (infoCheck . property) examples)

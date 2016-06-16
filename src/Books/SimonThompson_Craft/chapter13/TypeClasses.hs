
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

instance Info Char where
    examples = ['a', 'A', 'z', 'Z', '0', '9']
    size _   = 1 -- note each char has size 1.

instance Info Bool where
    examples = [True, False]
    size _   = 1

instance Info Int where
    examples = [-100 .. 100]
    size _   = 1



data Shape = Circle Float | Rectangle Float Float deriving (Eq, Ord, Show)


area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle l w) = l * w


-- HELP how to write this if it were (Shape a)?
instance Info Shape  where
    examples = [Circle 3.0, Rectangle 45.9 87.6]
    size     = round . area







-- Instances and contexts ----------

-- note key - defining examples and size over [a] type
instance Info a => Info [a] where
    -- note has type [[a]]
    examples = [ [] ] ++
               [ [x] | x <- examples ] ++
               [ [x,y] | x <- examples, y <- examples ]
    -- note has type [a]
    size     = foldr (+) 1 . map size
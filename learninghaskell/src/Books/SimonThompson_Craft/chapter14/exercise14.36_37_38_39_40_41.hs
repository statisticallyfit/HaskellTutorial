


data Shape = Circle a | Rectangle a b | Triangle a deriving (Eq, Show)

data Drawable = Drawable Shape deriving (Eq, Show)

data Scalable = ScaleBy Double deriving (Eq, Show)

-- HElp multiple copies of objects?

data Vector = Vec Float Float deriving (Eq, Show)
data Point = Point Float Float deriving (Eq, Show)
data Figure = Line Point Point | Circle Point Float deriving (Eq, Show)

class Movable a where
    move      :: Vector -> a -> a
    reflectX  :: a -> a
    reflectY  :: a -> a
    rotate180 :: a -> a
    rotate180 = reflectX . reflectY

instance Movable Point where
    move (Vec v1 v2) (Point x y) = Point (x + v1) (y + v2)
    reflectX (Point x y) = Point x (-y)
    reflectY (Point x y) = Point (-x) y
    rotate180 (Point x y) = Point (-x) (-y) -- more efficient to override with this def.

instance Movable Figure where
    move v (Line p1 p2) = Line (move v p1) (move v p2)
    move v (Circle p r) = Circle (move v p) r

    reflectX (Line p1 p2) = Line (reflectX p1) (reflectX p2)
    reflectX (Circle p r) = Circle (reflectX p) r

    reflectY (Line p1 p2) = Line (reflectY p1) (reflectY p2)
    reflectY (Circle p r) = Circle (reflectY p) r


instance Movable a => Movable [a] where
    move v = map (move v) -- list of points/circles/lines
    reflectX = map reflectX -- list here
    reflectY = map reflectY -- list here

----------------------------------------------------------------

data Name a = Pair a String deriving (Eq, Show)

class Named a where
    lookName :: a -> String
    putName  :: String -> a -> a

instance Named (Name a) where
    lookName (Pair obj nm) = nm
    putName nm (Pair obj _) = Pair obj nm


mapName :: (a -> b) -> Name a -> Name b
mapName f (Pair obj nm) = Pair (f obj) nm

-- note adding names to the movable objects.
{-
-- HELP question - given that (a) obeys Movable, how is it that (a) now
-- goes as argument to Name and that Name a goes as arg to Movable?
-- What does this mean?
instance Movable a => Movable (Name a) where
    move v = mapName (move v)  -- the name pair arg here
    reflectX = mapName reflectX -- the name pair arg here
    reflectY = mapName reflectY -- the name pair arg here
-}






-- exercise 37 -----------------------------------------------------------------------

{-
NOTE
instead of this, ...
instance Movable a => NamedMovable (Name a)
give ...
instance (Movable b, Named c) => NamedMovable (b,c)
by giving the below instances: ...
-}


-- HELP HELP TODO

class (Movable b, Named b) => NamedMovable b


instance Movable b => Movable (b, c) where


instance Named c => Named (b, c) where
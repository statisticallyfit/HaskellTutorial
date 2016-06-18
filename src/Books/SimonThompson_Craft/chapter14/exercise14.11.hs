data NTree = NilT | Node Integer NTree NTree deriving (Eq, Show)

t1, t2 :: NTree
t1 = Node 10 NilT NilT
t2 = Node 17 (Node 14 NilT NilT) (Node 20 NilT NilT)
t3 = Node 3 (Node 4 NilT NilT) NilT


maxNode :: NTree -> Maybe Integer
maxNode tree = case maxN tree of
                    -999 -> Nothing
                    num -> Just num

maxN :: NTree -> Integer
maxN NilT = -999
maxN (Node n t1 t2) = maximum [n, maxN t1, maxN t2]




minNode :: NTree -> Maybe Integer
minNode tree = case minN tree of
                    -999 -> Nothing
                    num -> Just num

minN :: NTree -> Integer
minN NilT = 999
minN (Node n t1 t2) = minimum [n, minN t1, minN t2]



-- HELP what is a better way to return a signal value from minNode/maxNode?
-- if I use -999 in minN then Nothing is returned from minNode (e.g.) so this signaling
-- must be changed.
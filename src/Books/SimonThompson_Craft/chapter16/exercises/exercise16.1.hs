
type Var = Char
newtype Store = Store [(Var, Integer)]



initial :: Store
initial = Store []

value :: Store -> Var -> Integer
value (Store []) v = 0
value (Store ((w,n) : rest)) v
    | v == w  = n
    | otherwise = value (Store rest) v


-- note insert the new pair in order alphabetically (according to Var).
update :: Store -> Var -> Integer -> Store
update (Store sto) v n
    | isValid (Store sto) = Store (insertPair (v,n) sto)
    | otherwise = Store (insertPair (v,n) validSto)
    where Store validSto = makeValid (Store sto)



-- note is it sorted in ascending order?
isValid :: Store -> Bool
isValid (Store sto) = isSorted sto
    where isSorted [] = True
          isSorted [a] = True
          isSorted (a:b:cs)
            | a > b = False
            | otherwise = True && isSorted (b:cs)

-- note order ascendingly
makeValid :: Store -> Store
makeValid (Store sto) = Store (sortByFst sto)
    where sortByFst [] = []
          sortByFst (p:ps) = sortByFst [pair | pair <- ps, pair <= p] ++
                             [p] ++
                             sortByFst [pair | pair <- ps, pair > p]

-- precondition: the list y:ys is sorted ascendingly
insertPair :: (Var, Integer) -> [(Var, Integer)] -> [(Var, Integer)]
insertPair p []     = [p]
insertPair p1 (p2 : ps)
    | p2 `greaterOrEqual` p1 = p1 : p2 : ps
    | otherwise = p2 : insertPair p1 ps
    where greaterOrEqual (x1,y1) (x2,y2) = x1 > x2 || (x1 == x2 && y1 >= y2)



instance Eq Store where
    (Store s1) == (Store s2) = (s1 == s2)

instance Show Store where
    showsPrec n (Store s) = showsPrec n s





sto1 = Store [('a',10), ('b',2), ('b',2), ('c',5), ('d',6), ('j',0), ('k',99), ('k',101)]
sto2 = Store [('z', 10), ('z', 3), ('p', 2), ('m', 8), ('n', 7), ('a', 5), ('b', 3)]
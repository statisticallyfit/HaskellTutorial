import Data.Maybe

{-
Precondition to all methods: tree given MUST be a binary search tree! (in order).
-}

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq, Show)


t1 :: Tree Integer
t1 = Node 8 (Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil))
                    (Node 6 (Node 5 Nil Nil) (Node 7 Nil Nil)))
            (Node 10 (Node 9 Nil Nil) (Node 11 Nil Nil))

t2 :: Tree Integer
t2 = Node 8 (Node 4 (Node 2 Nil (Node 3 Nil Nil))
                    (Node 6 (Node 5 Nil Nil) (Node 7 Nil Nil)))
            (Node 10 (Node 9 Nil Nil) (Node 11 Nil Nil))

t123 = Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)
t567 = Node 6 (Node 5 Nil Nil) (Node 7 Nil Nil)

-- deleting 3 from these trees.
testDelete1 = Node 2 (Node 1 Nil Nil) (Node 3 Nil (Node 4 Nil Nil))
testDelete2 = Node 1 (Node 0 Nil Nil) (Node 3 (Node 2 Nil Nil) Nil)
testDelete3 = Node 1 (Node 0 Nil Nil) (Node 3 (Node 2 Nil Nil) (Node 4 Nil Nil))

-- testing closest with different distanced nodes
t3 = Node 8 (Node 5  (Node 1 Nil Nil)  (Node 6 Nil Nil))
            (Node 20 (Node 19 Nil Nil) (Node 21 Nil Nil))



-- insTree and delete are not inverse functions.
insTree :: Ord a => a -> Tree a -> Tree a
insTree val Nil = Node val Nil Nil
insTree val (Node v t1 t2)
    | v == val = Node v t1 t2
    | val > v  = Node v t1 (insTree val t2)
    | val < v  = Node v (insTree val t1) t2

delete :: Ord a => a -> Tree a -> Tree a
delete val (Node v t1 t2)
    | val < v   = Node v (delete val t1) t2
    | val > v   = Node v t1 (delete val t2)
    | isNil t2  = t1 -- so in these leftover 3 tests (val == v)
    | isNil t1  = t2
    | otherwise = join t1 t2

minTree :: Ord a => Tree a -> Maybe a
minTree t
    | isNil t = Nothing
    | isNil t1 = Just v
    | otherwise = minTree t1
    where t1 = leftSub t
          v = treeVal t

-- note is auxiliary, not exported.
-- postcondition: all elements on left are smaller than those on right.
join :: Ord a => Tree a -> Tree a -> Tree a
join t1 t2 = Node miniVal t1 newTree
    where (Just miniVal) = minTree t2
          newTree = delete miniVal t2


isNil :: Tree a -> Bool
isNil Nil = True
isNil _ = False

isNode :: Tree a -> Bool
isNode Nil = False
isNode _ = True

leftSub :: Tree a -> Tree a
leftSub Nil = error "leftSub"
leftSub (Node _ t1 _) = t1

rightSub :: Tree a -> Tree a
rightSub Nil = error "rightSub"
rightSub (Node _ _ t2) = t2

treeVal :: Tree a -> a
treeVal Nil = error "treeVal"
treeVal (Node v _ _) = v


-- note return nth element of search tree.
-- HELP understand how the (n- tsize1 - 1) thingy works.
-- It can be used in situations like indexTree 3 t2 ==> 5.
indexTree :: Int -> Tree a -> a
indexTree n t
    | isNil t = error "indexTree"
    | n < tSize1 = indexTree n t1
    | n == tSize1 = v
    | otherwise = indexTree (n - tSize1 - 1) t2
    where v = treeVal t
          t1 = leftSub t
          t2 = rightSub t
          tSize1 = size t1

size :: Tree a -> Int
size t
    | isNil t = 0
    | otherwise = 1 + size (leftSub t) + size (rightSub t)



occurs :: Ord a => a -> Tree a -> Bool
occurs val Nil = False
occurs val (Node v t1 t2)
    | val == v = True
    | val < v = occurs val t1
    | otherwise = occurs val t2





-- exercise 29 ---------------------------------------------------------

-- precondition: given value must occur at least once in tree.
predecessor :: (Ord a, Num a) => a -> Tree a -> Maybe a
predecessor val t = if occurs val t then (pre val t) else Nothing
    where pre val Nil = Nothing
          pre val (Node v t1 t2)
            | v < val = Just v
            | otherwise = pre val t1


successor :: (Ord a, Num a) => a -> Tree a -> Maybe a
successor val t = if occurs val t then (succ val t) else Nothing
    where succ val Nil = Nothing
          succ val (Node v t1 t2)
            | v == val + 1 = Just v
            | val < v = succ val t1
            | otherwise = succ val t2 -- note val >= v only

-- note returns value in t which has smallest numerical difference from v.
-- precondition: value val must occur in tree given.
closest :: Integer -> Tree Integer -> Maybe [Integer]
closest val Nil = Nothing
closest val t = if occurs val t then clos val t else Nothing
    where
    clos val t
        | length allJusts == 1 = onlyOne
        | otherwise            = closerOne
        where (pm, sm)  = (predecessor val t, successor val t)
              allJusts  = filter isJust [pm, sm]
              onlyOne   = Just [fromJust (head allJusts)]
              vs@[p, s] = catMaybes allJusts -- both values p and s
              [d1, d2]  = map (\v -> abs(v - val)) vs
              closerOne
                | d1 == d2 = Just [p, s]
                | d1 < d2  = Just [p]
                | d1 > d2  = Just [s]
{-

    | bothJust pm sm = closerOne pm sm
    | oneJust pm sm = returnJust pm sm
    | noneJust pm sm = Nothing
    where pm = predecessor val t
          sm = successor val t
          bothJust pm sm = isJust pm && isJust sm
          oneJust pm sm = isJust pm || isJust sm
          noneJust pm sm = isNothing pm && isNothing sm
          closerOne pm sm =

    | any isJust [pm, sm] = if length vs == 1 then (Just $ vs) else closerOne
    | otherwise = Nothing
    where pm = predecessor val t
          sm = successor val t
          vs = catMaybes [pm, sm]
          [d1, d2] = map (\v -> abs(v - val)) vs
          closerOne
            | d1 == d2  = Just [p, s]
            | d1 < d2   = Just [p]
            | otherwise = Just [s]
            where p = head vs
                  s = tail vs
-}




{-

closest val t = case predecessor val t of
                    Nothing -> Nothing
                    Just p -> case successor val t of
                                Nothing -> Nothing
                                Just s -> closerOne s p (smallestDiff s p val)
       where smallestDiff s p val = (abs (val - p), abs (val - s))
             closerOne s p (r1, r2)
                | r1 == r2 = Just [p, s]
                | r1 < r2 = Just [p]
                | otherwise = Just [s]
-}

------------------------------------------------------------------------
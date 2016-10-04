

{- NOTe all together (same times together)
percolate (Node "+" (Leaf (Num n)) (Leaf (Num m))) = Leaf $ Num $ n + m
percolate (Node "+" (Leaf (Num n)) right) = Node "+" Empty (perc (n +) right)
percolate (Node "+" left (Leaf (Num n))) = Node "+" (perc (n +) left) Empty
percolate (Node "+" leaf@(Leaf varOrFunc) right) = Node "+" leaf (perc id right)
percolate (Node "+" left right) = Node "+" (percolate left) (percolate right)

percolate (Node "-" (Leaf (Num n)) (Leaf (Num m))) = Leaf $ Num $ n - m
percolate (Node "-" (Leaf (Num n)) right) = Node "-" Empty (perc (n -) right)
percolate (Node "-" left (Leaf (Num n))) = Node "-" Empty (perc (\x -> x - n) left)
percolate (Node "-" leaf@(Leaf varOrFunc) right) = Node "+" leaf (perc id right)
percolate (Node "-" left right) = Node "-" (percolate left) (percolate right)

percolate (Node "*" (Leaf (Num n)) (Leaf (Num m))) = Leaf $ Num $ n * m
percolate (Node "*" (Leaf (Num n)) right) = Node "*" Empty (perc (n *) right)
percolate (Node "*" left (Leaf (Num n))) = Node "*" Empty (perc (n *) left)
percolate (Node "*" leaf@(Leaf varOrFunc) right) = Node "*" leaf (perc id right)
percolate (Node "*" left right) = Node "*" (percolate left) (percolate right)

percolate (Node "/" (Leaf (Num n)) (Leaf (Num m))) = Leaf $ Num $ n `div` m
percolate (Node "/" (Leaf (Num n)) right) = Node "/" Empty (perc (n `div`) right)
percolate (Node "/" left (Leaf (Num n))) = Node "/" Empty (perc (\x -> x `div` n) left)
percolate (Node "/" leaf@(Leaf varOrFunc) right) = Node "/" leaf (perc id right)
percolate (Node "/" left right) = Node "/" (percolate left) (percolate right)

percolate (Node "^" (Leaf (Num n)) (Leaf (Num m))) = Leaf $ Num $ n ^ m
percolate (Node "^" (Leaf (Num n)) right) = Node "^" Empty (perc (n ^) right)
percolate (Node "^" left (Leaf (Num n))) = Node "^" Empty (perc (\x -> x ^ n) left)
percolate (Node "^" leaf@(Leaf varOrFunc) right) = Node "^" leaf (perc id right)
percolate (Node "^" left right) = Node "^" (percolate left) (percolate right)
-}
{-
--- crossed num-num
--- Crossed
perc f (Node op (Leaf (Num n)) (Leaf (Num m)))
    | op == "+" = Leaf (Num ((f n) + m))
    | op == "-" = Leaf (Num ((f n) - m))
    | op == "*" = Leaf (Num ((f n) * m))
    | op == "/" = Leaf (Num ((f n) `div` m))
    | op == "^" = Leaf (Num ((f n) ^ m))
perc f (Node op (Leaf (Num n)) leaf@(Leaf varOrFunc))
    | op == "+" = Node "+" (Leaf (Num (f n))) leaf
    | op == "-" = Node "-" (Leaf (Num (f n))) leaf
    | op == "*" = Node "*" (Leaf (Num (f n))) leaf
    | op == "/" = Node "/" (Leaf (Num (f n))) leaf
    | op == "^" = Node "^" (Leaf (Num (f n))) leaf
--- Crossed
perc f (Node op leaf@(Leaf varOrFunc) (Leaf (Num m)))
    | op == "+" = Node "+" leaf (Leaf (Num (f m)))
    | op == "-" = Node "-" leaf (Leaf (Num (f m)))
    | op == "*" = Node "*" leaf (Leaf (Num (f m)))
    | op == "/" = Node "/" leaf (Leaf (Num (f m)))
    | op == "^" = Node "^" leaf (Leaf (Num (f m)))
--- crossed var var
--- crossed
perc f (Node op (Leaf (Num n)) right)
    | op == "+" = Node "+" Empty (perc ((f n) +) right)
    | op == "-" = Node "-" Empty (perc ((f n) -) right)
    | op == "*" = Node "*" Empty (perc ((f n) *) right)
    | op == "/" = Node "/" Empty (perc ((f n) `div`) right)
    | op == "^" = Node "^" Empty (perc ((f n) ^) right)
--- crossed
perc f (Node op left (Leaf (Num m)))
    | op == "+" = Node "+" (perc (\x -> x + (f m)) left) Empty
    | op == "-" = Node "-" (perc (\x -> x - (f m)) left) Empty
    | op == "*" = Node "*" (perc (\x -> x * (f m)) left) Empty
    | op == "/" = Node "/" (perc (\x -> x `div` (f m)) left) Empty
    | op == "^" = Node "^" (perc (\x -> x ^ (f m)) left) Empty
--- crossed
perc f (Node op leaf@(Leaf varOrFunc) right)
    | op == "+" = Node "+" leaf (perc f right)
    | op == "-" = Node "-" leaf (perc f right)
    | op == "*" = Node "*" leaf (perc f right)
    | op == "/" = Node "/" leaf (perc f right)
    | op == "^" = Node "^" leaf (perc f right)
--- crossed
perc f (Node op left leaf@(Leaf varOrFunc))
    | op == "+" = Node "+" (perc f left) leaf
    | op == "-" = Node "-" (perc f left) leaf
    | op == "*" = Node "*" (perc f left) leaf
    | op == "/" = Node "/" (perc f left) leaf
    | op == "^" = Node "^" (perc f left) leaf
-}
{-
-- note: the meat add cases
perc f (Node "+" (Leaf (Num n)) leaf@(Leaf varOrFunc)) = Node "+" (Leaf (Num (f n))) leaf
perc f (Node "+" leaf@(Leaf varOrFunc) (Leaf (Num m))) = Node "+" leaf (Leaf (Num (f m)))
perc f (Node "+" (Leaf (Num n)) right) = Node "+" Empty (perc ((f n) +) right)
perc f (Node "+" left (Leaf (Num m))) = Node "+" (perc (\x -> x + (f m)) left) Empty
perc f (Node "+" leaf@(Leaf varOrFunc) right) = Node "+" leaf (perc f right)
perc f (Node "+" left leaf@(Leaf varOrFunc)) = Node "+" (perc f left) leaf
-- note: the meat sub cases
perc f (Node "-" (Leaf (Num n)) leaf@(Leaf varOrFunc)) = Node "-" (Leaf (Num (f n))) leaf
perc f (Node "-" leaf@(Leaf varOrFunc) (Leaf (Num m))) = Node "-" leaf (Leaf (Num (f m)))
perc f (Node "-" (Leaf (Num n)) right) = Node "-" Empty (perc ((f n) -) right)
perc f (Node "-" left (Leaf (Num m))) = Node "-" (perc (\x -> x - (f m)) left) Empty
perc f (Node "-" leaf@(Leaf varOrFunc) right) = Node "-" leaf (perc f right)
perc f (Node "-" left leaf@(Leaf varOrFunc)) = Node "-" (perc f left) leaf
-- note: the meat mult cases
perc f (Node "*" (Leaf (Num n)) leaf@(Leaf varOrFunc)) = Node "*" (Leaf (Num (f n))) leaf
perc f (Node "*" leaf@(Leaf varOrFunc) (Leaf (Num m))) = Node "*" leaf (Leaf (Num (f m)))
perc f (Node "*" (Leaf (Num n)) right) = Node "*" Empty (perc ((f n) *) right)
perc f (Node "*" left (Leaf (Num m))) = Node "*" (perc ((f m) *) left) Empty
perc f (Node "*" leaf@(Leaf varOrFunc) right) = Node "*" leaf (perc f right)
perc f (Node "*" left leaf@(Leaf varOrFunc)) = Node "*" (perc f left) leaf
-- note: the meat div cases
perc f (Node "/" (Leaf (Num n)) leaf@(Leaf varOrFunc)) = Node "/" (Leaf (Num (f n))) leaf
perc f (Node "/" leaf@(Leaf varOrFunc) (Leaf (Num m))) = Node "/" leaf (Leaf (Num (f m)))
perc f (Node "/" (Leaf (Num n)) right) = Node "/" Empty (perc ((f n) `div`) right)
perc f (Node "/" left (Leaf (Num m))) = Node "/" (perc (\x -> x `div` (f m)) left) Empty
perc f (Node "/" leaf@(Leaf varOrFunc) right) = Node "/" leaf (perc f right)
perc f (Node "/" left leaf@(Leaf varOrFunc)) = Node "/" (perc f left) leaf
-- note: the meat pow cases
perc f (Node "^" (Leaf (Num n)) leaf@(Leaf varOrFunc)) = Node "^" (Leaf (Num (f n))) leaf
perc f (Node "^" leaf@(Leaf varOrFunc) (Leaf (Num m))) = Node "^" leaf (Leaf (Num (f m)))
perc f (Node "^" (Leaf (Num n)) right) = Node "^" Empty (perc ((f n) ^) right)
perc f (Node "^" left (Leaf (Num m))) = Node "^" (perc (\x -> x ^ (f m)) left) Empty
perc f (Node "^" leaf@(Leaf varOrFunc) right) = Node "^" leaf (perc f right)
perc f (Node "^" left leaf@(Leaf varOrFunc)) = Node "^" (perc f left) leaf
-}

{-
squash :: Tree Expr -> Tree Expr
squash node@(Node op (Leaf l1) (Leaf l2)) = node
squash (Node op Empty leaf@(Leaf l)) = leaf
squash (Node op Empty right) = squash right
squash (Node op leaf@(Leaf l) right) = Node op leaf (squash right)
-}






{-

-- note yields in order results
foldrIn :: (a -> b -> b) -> b -> Tree a -> b
foldrIn _ acc Nil = acc
foldrIn f acc (Node n left right) = foldrIn f (f n (foldrIn f acc right)) left

-- note if you want forward inorder, not backwards, put right as inner and left outer.
foldlIn :: (b -> a -> b) -> b -> Tree a -> b
foldlIn _ acc Nil = acc
foldlIn f acc (Node n left right) = foldlIn f (f (foldlIn f acc right) n) left

foldrPost :: (a -> b -> b) -> b -> Tree a -> b
foldrPost _ acc Nil = acc
foldrPost f acc (Node n left right) = foldrPost f (foldrPost f (f n acc) right) left

foldlPost :: (b -> a -> b) -> b -> Tree a -> b
foldlPost _ acc Nil = acc
foldlPost f acc (Node n left right) = foldlPost f (foldlPost f (f acc n) right) left

-- todo fix so they print in order
foldrPre :: (a -> b -> b) -> b -> Tree a -> b
foldrPre _ acc Nil = acc
foldrPre f acc (Node n left right) = foldrPre f (foldrPre f (f n acc) left) right

-- todo fix so it prints in order
-- foldl f (foldl f (f z n) left) right
foldlPre :: (b -> a -> b) -> b -> Tree a -> b
foldlPre _ acc Nil = acc
foldlPre f acc (Node n left right) = foldlPre f (foldlPre f (f acc n) left) right
-}


{-

traverseTree :: (a -> (b -> b) -> (b -> b) -> b -> b) -> (t -> a) -> b -> Tree t -> b
traverseTree step f z tree = go tree z
    where
    go Empty z = z
    go (Leaf x) z = step (f x) (go Empty) (go Empty) z
    go (Node x l r) z = step (f x) (go l) (go r) z


preorder, inorder, postorder :: (a -> b -> b) -> b -> Tree a -> b
preorder   = traverseTree $ \n l r -> n . l . r  -- r . l . n
inorder    = traverseTree $ \n l r -> l . n . r  -- r . n . l
postorder  = traverseTree $ \n l r -> l . r . n  -- n . r . l
-}




-- (f . (n * ))
-- example: foldLeftTree $ getConsts() $ 4xsin(x)3cos(x) => 12
--foldLeftTree :: {-(Int -> Int -> Int) ->-} Int -> Tree Expr -> Int
--foldLeftTree s (Leaf(Num n)) = s n
{-foldLeftTree s (Node "*" (Leaf(Num n)) right) = foldLeftTree (s * n) right
foldLeftTree s (Node "/" (Leaf(Num n)) right) = foldLeftTree (s `div` n) right
foldLeftTree s (Node _ (Leaf X) right) = foldLeftTree s right
foldLeftTree s (Leaf (Num n)) = -}
--foldLeftTree f (Node op left right) = foldLeftTree (f . )



{-
main :: IO()
main = do
    print $ foldLeftTree (\x -> )-}


{-

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)


instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node left a right) = (foldMap f left) <> (f a) <> (foldMap f right)

    foldl _ z Empty = z
    foldl f z (Leaf a) = f z a
    foldl f z (Node left a right) = foldl f (foldl f (f z a) left) right

    foldr _ z Empty = z
    foldr f z (Leaf a) = f a z
    foldr f z (Node left a right) = foldr f (f a (foldr f z right)) left
-}
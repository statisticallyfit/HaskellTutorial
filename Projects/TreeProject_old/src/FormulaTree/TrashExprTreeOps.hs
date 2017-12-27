

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


------
-- showing examples





{-

instance Show Expr where
    show expr
        | isMono expr = concatMap show $ split MulOp expr
        | otherwise = show' expr
        where
        show' X = "x"
        show' Y = "y"
        show' (Num n) = show n
        show' (F func) = show func
        show' (Add e1 e2) = show' e1 ++ " + " ++ show' e2
        show' (Sub e1 e2) = show' e1 ++ " - " ++ show' e2

        show' (Mul (Num n) (Num m)) = "(" ++ show n ++ ")(" ++ show m ++ ")"
        show' (Mul (Num n) p@(Pow _ _)) = show n ++ show' p
        show' (Mul (Num n) X) = show n ++ show' X
        show' (Mul (Neg (Num n)) X) = "-" ++ show n ++ show' X
        show' (Neg (Mul (Num n) X)) = "-" ++ show n ++ show' X
        show' (Mul (Num n) rest) = show n ++ "(" ++ show' rest ++ ")"

        show' (Mul (Mul (Mul (Mul rest p1@(Pow _ _)) p2@(Pow _ _)) p3@(Pow _ _)) p4@(Pow _ _))
            = if isPow rest
            then "(" ++ show' rest ++ ")(" ++ show' p1 ++ ")(" ++ show' p2 ++ ")(" ++ show' p3 ++ ")(" ++ show' p4 ++ ")"
            else show' rest ++ "(" ++ show' p1 ++ ")(" ++ show' p2 ++ ")(" ++ show' p3 ++ ")(" ++ show' p4 ++ ")"
        show' (Mul (Mul (Mul rest p1@(Pow _ _)) p2@(Pow _ _)) p3@(Pow _ _))
            = if isPow rest
            then "(" ++ show' rest ++ ")(" ++ show' p1 ++ ")(" ++ show' p2 ++ ")(" ++ show' p3 ++ ")"
            else show' rest ++ "(" ++ show' p1 ++ ")(" ++ show' p2 ++ ")(" ++ show' p3 ++ ")"
        show' (Mul (Mul rest p1@(Pow _ _)) p2@(Pow _ _))
            = if isPow rest
            then "(" ++ show' rest ++ ")(" ++ show' p1 ++ ")(" ++ show' p2 ++ ")"
            else show' rest ++ "(" ++ show' p1 ++ ")(" ++ show' p2 ++ ")"
        show' (Mul rest p@(Pow _ _))
            = if isPow rest
            then "(" ++ show' rest ++ ")(" ++ show' p ++ ")"
            else show' rest ++ "(" ++ show' p ++ ")"


        show' (Mul (Mul (Mul (Mul rest (Num n1)) (Num n2)) (Num n3)) (Num n4))
            = if isNum rest
            then "(" ++ show rest ++ ")(" ++ show n1 ++ ")(" ++ show n2 ++ ")(" ++ show n3 ++ ")(" ++ show n4 ++ ")"
            else show' rest ++ "(" ++ show n1 ++ ")(" ++ show n2 ++ ")(" ++ show n3 ++ ")(" ++ show n4 ++ ")"
        show' (Mul (Mul (Mul rest (Num n1)) (Num n2)) (Num n3))
            = if isNum rest
            then "(" ++ show rest ++ ")(" ++ show n1 ++ ")(" ++ show n2 ++ ")(" ++ show n3 ++ ")"
            else show' rest ++ "(" ++ show n1 ++ ")(" ++ show n2 ++ ")(" ++ show n3 ++ ")"
        show' (Mul (Mul rest (Num n1)) (Num n2))
            = if isNum rest
            then "(" ++ show rest ++ ")(" ++ show n1 ++ ")(" ++ show n2 ++ ")"
            else show' rest ++ "(" ++ show n1 ++ ")(" ++ show n2 ++ ")"
        show' (Mul rest (Num n))
            = if isNum rest
            then "(" ++ show rest ++ ")(" ++ show n ++ ")"
            else show' rest ++ "(" ++ show n ++ ")"

        show' (Mul d1@(Div _ _) d2@(Div _ _)) = "(" ++ show' d1 ++ ") (" ++ show' d2 ++ ")"
        show' (Mul d@(Div _ _) m@(Mul _ _)) = "(" ++ show' d ++ ") (" ++ show' m ++ ")"
        show' (Mul m@(Mul _ _) d@(Div _ _)) = "(" ++ show' m ++ ") (" ++ show' d ++ ")"
        show' (Mul m1@(Mul (Num a) (Pow _ _)) m2@(Mul (Num b) (Pow _ _)))
            = "(" ++ show' m1 ++ ") (" ++ show' m2 ++ ")"
        show' (Mul e1 a@(Add _ _)) = show' e1 ++ "(" ++ show' a ++ ")"
        show' (Mul e1 ng@(Neg (Num n))) = show' e1 ++ "(" ++ show' ng ++ ")"
        show' (Mul e1 e2)
            = if isPow e1 && isPow e2
             then show' e1 ++ " * " ++ show' e2
             else if isPow e1 && isNum e2
             then "(" ++ show' e1 ++ ")(" ++ show' e2 ++ ")"
             else show' e1 ++ show' e2

        show' (Div (Num n) (Num m)) = show n ++ "/" ++ show m
        show' (Div e1 e2) = "(" ++ show' e1 ++ ") / (" ++ show' e2 ++ ")"
        show' (Pow x d@(Div (Num a) (Num b))) = show' x ++ "^(" ++ show' d ++ ")"
        show' (Pow x d@(Div e1 e2)) = show' x ++ "^" ++ show' d
        show' (Pow e1 e2) = show' e1 ++ "^" ++ show' e2

        show' (Neg m@(Mul _ _)) = "-" ++ show' m
        show' (Neg d@(Div _ _)) = "-" ++ show' d
        show' (Neg p@(Pow _ _)) = "-" ++ show' p
        show' (Neg f@(F _)) = "-" ++ show' f
        show' (Neg X) = "-" ++ show' X
        show' (Neg Y) = "-" ++ show' Y
        show' (Neg e) = "-(" ++ show' e ++ ")"
-}



-- NOTE the changed one with Mono testing but doesn't work.
{-
instance Show Expr where
    show X = "x"
    show Y = "y"
    show (Num n) = show n
    show (F func) = show func
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Sub e1 e2) = show e1 ++ " - " ++ show e2

    show (Mul (Num n) (Num m)) = "(" ++ show n ++ ")(" ++ show m ++ ")"
    show (Mul (Num n) p@(Pow _ _)) = show n ++ show p
    show (Mul (Num n) X) = show n ++ show X
    show (Mul (Neg (Num n)) X) = "-" ++ show n ++ show X
    --show (Neg (Mul (Num n) X)) = "-" ++ show n ++ show X
    show (Mul (Num n) Y) = show n ++ show Y
    show (Mul (Neg (Num n)) Y) = "-" ++ show n ++ show Y
    --show (Neg (Mul (Num n) Y)) = "-" ++ show n ++ show Y
    show (Mul (Num n) rest) = show n ++ "(" ++ show rest ++ ")"

    show (Mul (Mul (Mul (Mul rest p1@(Pow _ _)) p2@(Pow _ _)) p3@(Pow _ _)) p4@(Pow _ _))
        = if isPow rest
        then "(" ++ show rest ++ ")(" ++ show p1 ++ ")(" ++ show p2 ++ ")(" ++ show p3 ++ ")(" ++ show p4 ++ ")"
        else show rest ++ "(" ++ show p1 ++ ")(" ++ show p2 ++ ")(" ++ show p3 ++ ")(" ++ show p4 ++ ")"
    show (Mul (Mul (Mul rest p1@(Pow _ _)) p2@(Pow _ _)) p3@(Pow _ _))
        = if isPow rest
        then "(" ++ show rest ++ ")(" ++ show p1 ++ ")(" ++ show p2 ++ ")(" ++ show p3 ++ ")"
        else show rest ++ "(" ++ show p1 ++ ")(" ++ show p2 ++ ")(" ++ show p3 ++ ")"
    show (Mul (Mul rest p1@(Pow _ _)) p2@(Pow _ _))
        = if isPow rest
        then "(" ++ show rest ++ ")(" ++ show p1 ++ ")(" ++ show p2 ++ ")"
        else show rest ++ "(" ++ show p1 ++ ")(" ++ show p2 ++ ")"
    show (Mul rest p@(Pow _ _))
        = if isPow rest
        then "(" ++ show rest ++ ")(" ++ show p ++ ")"
        else show rest ++ "(" ++ show p ++ ")"


    show (Mul (Mul (Mul (Mul rest (Num n1)) (Num n2)) (Num n3)) (Num n4))
        = if isNum rest
        then "(" ++ show rest ++ ")(" ++ show n1 ++ ")(" ++ show n2 ++ ")(" ++ show n3 ++ ")(" ++ show n4 ++ ")"
        else show rest ++ "(" ++ show n1 ++ ")(" ++ show n2 ++ ")(" ++ show n3 ++ ")(" ++ show n4 ++ ")"
    show (Mul (Mul (Mul rest (Num n1)) (Num n2)) (Num n3))
        = if isNum rest
        then "(" ++ show rest ++ ")(" ++ show n1 ++ ")(" ++ show n2 ++ ")(" ++ show n3 ++ ")"
        else show rest ++ "(" ++ show n1 ++ ")(" ++ show n2 ++ ")(" ++ show n3 ++ ")"
    show (Mul (Mul rest (Num n1)) (Num n2))
        = if isNum rest
        then "(" ++ show rest ++ ")(" ++ show n1 ++ ")(" ++ show n2 ++ ")"
        else show rest ++ "(" ++ show n1 ++ ")(" ++ show n2 ++ ")"
    show (Mul rest (Num n))
        = if isNum rest
        then "(" ++ show rest ++ ")(" ++ show n ++ ")"
        else show rest ++ "(" ++ show n ++ ")"

    show (Mul d1@(Div _ _) d2@(Div _ _)) = "(" ++ show d1 ++ ") (" ++ show d2 ++ ")"
    show (Mul d@(Div _ _) m@(Mul _ _)) = "(" ++ show d ++ ") (" ++ show m ++ ")"
    show (Mul m@(Mul _ _) d@(Div _ _)) = "(" ++ show m ++ ") (" ++ show d ++ ")"
    show (Mul m1@(Mul (Num a) (Pow _ _)) m2@(Mul (Num b) (Pow _ _)))
        = "(" ++ show m1 ++ ") (" ++ show m2 ++ ")"
    show (Mul e1 a@(Add _ _)) = show e1 ++ "(" ++ show a ++ ")"
    show (Mul e1 ng@(Neg (Num n))) = show e1 ++ "(" ++ show ng ++ ")"
    show m@(Mul e1 e2)
        | isMono m = printMono m
        | isPow e1 && isPow e2 = show e1 ++ " * " ++ show e2
        | isPow e1 && isNum e2 = "(" ++ show e1 ++ ")(" ++ show e2 ++ ")"
        | otherwise = show e1 ++ show e2
        where printMono mono = concatMap show $ split MulOp mono

    show (Div (Num n) (Num m)) = show n ++ "/" ++ show m
    show d@(Div e1 e2) = if isMono d then (printMono d) else "(" ++ show e1 ++ ") / (" ++ show e2 ++ ")"
        where printMono mono = concatMap show $ split MulOp mono
    show (Pow x d@(Div (Num a) (Num b))) = show x ++ "^(" ++ show d ++ ")"
    show (Pow x d@(Div e1 e2)) = show x ++ "^" ++ show d
    show p@(Pow e1 e2) = if isMono p then (printMono p) else show e1 ++ "^" ++ show e2
        where printMono mono = concatMap show $ split MulOp mono

    show neg@(Neg e)
        | isMono neg = printMono neg
        | isMul e || isDiv e || isPow e || isFunction e = "-" ++ show e
        | otherwise = "-(" ++ show e ++ ")"
        where printMono mono = concatMap show $ split MulOp mono
-}


--- NOTE and show from good show function (only need one of num less
-- repetition)

    {-show (Mul (Mul (Mul (Mul rest p1@(Pow _ _)) p2@(Pow _ _)) p3@(Pow _ _)) p4@(Pow _ _))
        = if isPow rest
        then "(" ++ show rest ++ ")(" ++ show p1 ++ ")(" ++ show p2 ++ ")(" ++ show p3 ++ ")(" ++ show p4 ++ ")"
        else show rest ++ "(" ++ show p1 ++ ")(" ++ show p2 ++ ")(" ++ show p3 ++ ")(" ++ show p4 ++ ")"
    show (Mul (Mul (Mul rest p1@(Pow _ _)) p2@(Pow _ _)) p3@(Pow _ _))
        = if isPow rest
        then "(" ++ show rest ++ ")(" ++ show p1 ++ ")(" ++ show p2 ++ ")(" ++ show p3 ++ ")"
        else show rest ++ "(" ++ show p1 ++ ")(" ++ show p2 ++ ")(" ++ show p3 ++ ")"
    show (Mul (Mul rest p1@(Pow _ _)) p2@(Pow _ _))
        = if isPow rest
        then "(" ++ show rest ++ ")(" ++ show p1 ++ ")(" ++ show p2 ++ ")"
        else show rest ++ "(" ++ show p1 ++ ")(" ++ show p2 ++ ")"-}

{-
    show (Mul (Mul (Mul (Mul rest (Num n1)) (Num n2)) (Num n3)) (Num n4))
        = if isNum rest
        then "(" ++ show rest ++ ")(" ++ show n1 ++ ")(" ++ show n2 ++ ")(" ++ show n3 ++ ")(" ++ show n4 ++ ")"
        else show rest ++ "(" ++ show n1 ++ ")(" ++ show n2 ++ ")(" ++ show n3 ++ ")(" ++ show n4 ++ ")"
    show (Mul (Mul (Mul rest (Num n1)) (Num n2)) (Num n3))
        = if isNum rest
        then "(" ++ show rest ++ ")(" ++ show n1 ++ ")(" ++ show n2 ++ ")(" ++ show n3 ++ ")"
        else show rest ++ "(" ++ show n1 ++ ")(" ++ show n2 ++ ")(" ++ show n3 ++ ")"
    show (Mul (Mul rest (Num n1)) (Num n2))
        = if isNum rest
        then "(" ++ show rest ++ ")(" ++ show n1 ++ ")(" ++ show n2 ++ ")"
        else show rest ++ "(" ++ show n1 ++ ")(" ++ show n2 ++ ")" -}


-- TODO do show instance for odd and even:
-- Mul (Mul (Num 2) (Pow X (Num 3))) (Pow X (Num 6))






-- TODO major help why doesn't 4(x+3) simplify to 4x + 12, why 4x + (4)(3)?? ?
-- TODO probably because this has gotten too old - perhaps ther eis a case that goes ahead of the
-- one that should be entered that keeps it in this ugly state? Fix with printExpr and foldl.
{-
simplify :: Expr -> Expr
simplify (Var x) = Var x
simplify (Num n) = Num n
simplify (Frac f) = Frac f
simplify (F f) = F $ fmap simplify f

-- TODO may not need these cases once I do operations on expression holders of arrays.
simplify (Add (Num n) (Num m)) = Num $ n + m
simplify (Add (Mul (Num a) (Num b)) (Mul (Num c) (Num d))) = Num $ a * b + c + d
simplify (Add (Mul (Num a) x) (Mul (Num b) y))
    = if x == y then (Num (a + b) .* simplify x) else ((Num a) .* simplify x .+ (Num b) .* simplify y)
simplify (Add (Mul (Num a) x) (Mul y (Num b))) = simplify ((Num a) .* x .+ (Num b) .* y)
simplify (Add (Mul x (Num a)) (Mul (Num b) y)) = simplify ((Num a) .* x .+ (Num b) .* y)
simplify (Add (Mul x (Num a)) (Mul y (Num b))) = simplify ((Num a) .* x .+ (Num b) .* y)
simplify (Add rest (Mul (Num n) (Num m))) = simplify rest .+ (Num $ n * m)
simplify (Add (Neg e1) (Neg e2)) = simplify (Neg e1) .- simplify e2
simplify (Add (Neg e1) e2) = simplify (Neg e1) .+ simplify e2
simplify (Add e1 (Neg e2)) = simplify e1 .- simplify e2
simplify (Add x y) = if x == y then (Num 2 .* simplify x) else (simplify x .+ simplify y)

simplify (Sub (Num n) (Num m)) = Num $ n - m
simplify (Sub (Mul (Num a) (Num b)) (Mul (Num c) (Num d))) = Num $ a * b - c + d
simplify (Sub (Mul (Num a) x) (Mul (Num b) y))
    = if x == y then (Num (a - b) .* simplify x) else ((Num a) .* simplify x .- (Num b) .* simplify y)
simplify (Sub (Mul (Num a) x) (Mul y (Num b))) = simplify ((Num a) .* simplify x .- (Num b) .* simplify y)
simplify (Sub (Mul x (Num a)) (Mul (Num b) y)) = simplify ((Num a) .* simplify x .- (Num b) .* simplify y)
simplify (Sub (Mul x (Num a)) (Mul y (Num b))) = simplify ((Num a) .* simplify x .- (Num b) .* simplify y)
simplify (Sub (Neg e1) (Neg e2)) = simplify e1 .+ simplify e2
simplify (Sub (Neg e1) e2) = simplify (Neg e1) .- simplify e2
simplify (Sub e1 (Neg e2)) = simplify e1 .+ simplify e2
simplify (Sub x y) = if x == y then (Num 0) else (simplify x .- simplify y)

simplify (Mul (Num 1) e2) = simplify e2
simplify (Mul e1 (Num 1)) = simplify e1
simplify (Mul (Num 0) e2) = Num 0
simplify (Mul e1 (Num 0)) = Num 0
simplify (Mul (Num n) (Num m)) = Num $ n * m
simplify (Mul u (Num n)) = Num n .* simplify u
simplify (Mul (Neg u) (Neg v)) = simplify u .* simplify v
simplify (Mul (Neg u) v) = Neg (simplify u .* simplify v)
simplify (Mul u (Neg v)) = Neg (simplify u .* simplify v)
simplify (Mul e1 (Add x y)) = simplify e1 .* simplify x .+ simplify e1 .* simplify y
simplify m@(Mul (Mul (Num a) (Pow x (Num p)))
                (Mul (Num b) (Pow y (Num q))))
    = if x == y then (Num $ a * b) .* simplify x .^ (Num $ p + q) else simplify m

-}
{- TODO is this necessary?
        simplify (Mul a (Mul b rest))
            | isNum a && isNum b = a .* b .* simplify rest
            | negNum a && negNum b = a .* b .* simplify rest
            | negNum a && isNum b = Neg a .* b .* simplify rest
            | isNum a && negNum b = Neg a .* b .* simplify rest
            | otherwise = simplify a .* simplify b .* simplify rest
            where negNum x = isNegNumOrFrac x || isNumOrFracNeg x


        -- TODO after poly stuff fix this so that we simplify functions (go to simpFuncs partition)


        simplify (Mul (Mul (Mul rest f@(F _)) g@(F _)) h@(F _)) = simplify rest .* simplifyFunctions (f .* g .* h)
        simplify (Mul (Mul rest f@(F _)) g@(F _)) = simplify rest .* simplifyFunctions (f .* g)


        -- TODO now these functions are ready to go into the function simplifier (sin * cos * tan)

simplify (Mul (F (Sin u)) (F (Cos v)))
            = if u == v then F (Tan u) else (F (Sin $ simplify u)) .* (F (Cos $ simplify v))
        simplify (Mul t1@(F (Tan u)) t2@(F (Tan v)))
            = if u == v then (F (Tan u)) .^ Num 2 else simplify t1 .* simplify t2
        simplify (Mul (F (Sin u)) (F (Cos v))) -}{-


simplify poly@(Mul (Num n) maybePow)
    | n < 0 && isMono poly = Neg $ Mul (Num (-1 * n)) maybePow
    | n > 0 && isMono poly = poly
    | n < 0 = Mul (Neg (Num (-1*n))) maybePow
    | otherwise = Mul (Num n) (simplify maybePow)
simplify (Mul other (Div a b)) = Div (other .* a) b
simplify (Mul x y) = if x == y then simplify x .^ (Num 2) else simplify x .* simplify y

simplify (Div (Num 0) (Num 0)) = error "0/0 not possible"
simplify (Div (Num n) (Num m)) = Num $ n `div` m
simplify (Div (Num 0) e2) = Num 0
simplify (Div (Neg e1) (Neg e2)) = simplify $ simplify e1 ./ simplify e2
simplify (Div (Neg e1) e2) = Neg (simplify e1 ./ simplify e2)
simplify (Div e1 (Neg e2)) = Neg (simplify e1 ./ simplify e2)
simplify (Div e1 (Num 0)) = error "divide by zero!"
simplify (Div e1 (Num 1)) = simplify e1
simplify d@(Div (Mul (Num a) (Pow x (Num p)))
                (Mul (Num b) (Pow y (Num q))))
    = if x == y then (Num $ a `div` b) .* simplify x .^ (Num $ p - q) else simplify d
simplify (Div x y) = if x == y then (Num 1) else simplify x ./ simplify y

simplify (Pow (Num n) (Num m)) = Num $ n ^ m
simplify (Pow (Neg e1) (Neg e2)) = Num 1 ./ ((simplify (Neg e1)) .^ (simplify e2))
simplify (Pow (Neg e1) (Num p)) = if (even p) then (simplify e1 .^ Num p)
    else ((simplify (Neg e1)) .^ Num p)
simplify (Pow e1 (Neg e2)) = Num 1 ./ (simplify e1 .^ simplify e2)
simplify (Pow e (Num 0)) = Num 1
simplify (Pow (Num 0) e) = Num 0
simplify (Pow (Num 1) e) = Num 1
simplify (Pow e (Num 1)) = simplify e --- TODO do power rules next
simplify (Pow x y) = simplify x .^ simplify y

-- simplify (Neg (Num n)) = Num (-n)
simplify (Neg (Neg e)) = simplify e
simplify (Neg a@(Add e1 e2)) = simplify (Neg e1) .- simplify e2
simplify (Neg s@(Sub e1 e2)) = simplify (Neg e1) .+ simplify e2
simplify (Neg (Mul (Num n) x)) = Num (-n) .* simplify x
simplify (Neg m@(Mul _ _)) = Neg $ simplify m
simplify (Neg e) = Neg $ simplify e


simplifyComplete :: Expr -> Expr
simplifyComplete expr
    | s == expr = expr
    | otherwise = simplifyComplete $ simplify s
    where s = simplify expr
-}

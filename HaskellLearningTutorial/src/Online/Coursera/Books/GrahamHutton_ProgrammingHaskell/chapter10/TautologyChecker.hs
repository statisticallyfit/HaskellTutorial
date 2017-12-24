
{-
note: tautology is a logical proposition that is always true.

IMPLICATION
A  B     A => B
F  F       T
F  T       T
T  F       F
T  T       T
-}


data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop     -- or (||)
          | Equiv Prop Prop  -- equivalence <=> if and only if
        deriving (Eq, Ord, Show)

p1 :: Prop
p1 = And (Var 'A' ) (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B') )) (Var 'B')


p5 :: Prop
p5 = Or (Var 'A') (Var 'B')

p6 :: Prop
p6 = Equiv (Var 'A') (Var 'A')


p7 :: Prop
p7 = Equiv (Var 'A') (Var 'B')




type Assoc k v = [(k,v)]
-- substitution lookup take
type Subst = Assoc Char Bool



eval               :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equiv p q) = eval s p == eval s q



{- wrong
find :: Char -> Subst -> Bool
find x (a:as)
    | (fst a) == x = snd a
    | otherwise    = find x as
-}

find         :: Eq k => k -> Assoc k v -> v
find k tuple =  head [v | (k',v) <- tuple, k == k']

-- note returns the variables in a proposition
{-
So vars p2 = ['A', 'B', 'A']
-}
vars             :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q




intToBin :: Int -> [Int]
intToBin 0 = []
intToBin n = n `mod` 2 : intToBin (n `div` 2)


-- returns all possible combinations of bool values for a given length
-- take two copies of lists made by bools n just appending True and False for each set.
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

-- the same as counting in binary until that number given
bools' n = map (map conv . make n . intToBin) [0 .. limit]
          where
            limit = (2^n) - 1
            make n bs = take n (bs ++ repeat 0)
            conv 0 = False
            conv 1 = True


rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs )


-- generates all substitutions for a proposition by extracting its variables,
-- generating all possible bools for this length of vars, then zipping lists
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)


-- is tautology if evaluates to true for all possible substitutions
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

{-
note
substs p2
= [ [('A',False),('B',False)],
    [('A',False),('B',True)],
    [('A',True),('B',False)],
    [('A',True),('B',True)] ]

so when you write s <- substs p that means:

s = [('A', False), ('B', False)] for first time, then next one ....

eval s p
-}


main = do
    print p1; print $ isTaut p1
    print p2; print $ isTaut p2
    print p3; print $ isTaut p3
    print p4; print $ isTaut p4
    print p5; print $ isTaut p5
    print p6; print $ isTaut p6  -- exercise 5
    print p7; print $ isTaut p7  -- exercise 5
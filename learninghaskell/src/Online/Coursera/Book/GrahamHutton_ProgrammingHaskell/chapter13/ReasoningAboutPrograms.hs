
-- 13.4 INDUCTION ON NUMBERS --------------------------------------------------------

data Nat = Zero | Succ Nat deriving (Eq, Show)

add            :: Nat -> Nat -> Nat
add Zero m     = m
add (Succ n) m = Succ (add n m)

{-
example proof: commutativity in addition

1. base case: show that p Zero holds
add Zero Zero
= Zero

2. inductive case: show that i fp holds for any natural number n, then
p (Succ n) also holds. Assuming add n Zero = n then show add (Succ n) Zero = Succ n

add (Succ n) Zero
= Succ (add n Zero)
= Succ n


example: proof: associativity of addition (natural numbers)
add x (add y z) = add (add x y) z


1. base case:
add Zero (add y z)
= add y z
= add (add Zero y) z

2. inductive case:

add (Succ x) add (y z)
= Succ (add x (add y z))
= Succ (add (add x y) z)
= add (Succ (add x y) z)
= add (add (Succ x) y) z







-- 13.5 INDUCTION ON LISTS ---------------------------------------------------------

1. base case:

reverse (reverse [])
= reverse []
= []


2. inductive case:
assume -->  reverse (reverse xs) = xs
Show --> reverse (reverse x:xs)) = x:xs

reverse (reverse (x:xs))
= reverse (reverse xs ++ [x])
= reverse [x] ++ reverse (reverse xs) --- note distributivity
= [x] ++ reverse (reverse xs) -- note use induction hypothesis
= [x] ++ xs
= x:xs






NOTE HELP Defining reverse' by inductive proof:

reverse' xs ys = reverse xs ++ ys

1. base case:

reverse' [] ys
= reverse [] ++ ys -- specification
= [] ++ ys
= ys

2. inductive case

reverse' (x:xs) ys
= reverse (x:xs) ++ ys
= (reverse xs ++ [x]) ++ ys
= reverse xs ++ ([x] ++ ys)
= reverse' xs ([x] ++ ys) -- note help induction hypothesis
= reverse' xs (x:ys)

so this definition shows that reverse' xs ys = reverse xs ++ ys by induction

reverse'           :: [a] -> [a] -> [a]
reverse' [] ys     = ys
reverse' (x:xs) ys = reverse' xs (x:ys)



NOTE: reverse can be defined:

reverse :: [a] -> [a]
reverse xs = reverse' xs []

reverse [1,2,3]
= reverse' [1,2,3] []
= reverse' [2,3] (1:[])
= reverse' [3] (2:(1:[]))
= reverse' [] (3:(2:(1:[])))
= [3,2,1]







NOTE another example of how ++ can be eliminiated

data Tree = Leaf Int | Node Tree Tree

flatten            :: Tree -> [Int]
flatten (Leaf n)   = [n]
flatten (Node l r) = flatten l ++ flatten r


flatten' t ns = flatten t ++ ns


1. base case

flatten' (Leaf n) ns
= flatten (Leaf n) ++ ns
= [n] ++ ns
= n : ns

2. inductive case

flatten' (Node l r) ns
= flatten l ++ flatten r) ++ ns
= flatten l ++ (flatten r ++ ns)
= flatten' l (flatten r ++ ns)   note inductive hypothesis for l
= flatten' l (flatten' r ns)     note inductive hypothesis for r


so define:

flatten'               :: Tree -> [Int] -> [Int]
flatten' (Leaf n) ns   = n : ns
flatten' (Node l r) ns = flatten' l (flatten' r ns)

note original is changed

flatten   :: Tree -> [Int]
flatten t = flatten' t []  note uses extra arg to accumulate final result, not ++
-}


data Expr = Val Int | Add Expr Expr deriving (Eq, Show)

e1 = Add (Add (Val 2) (Val 3)) (Val 4)

eval           :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y

-- expressions can be evaluated also indirectly with a stack.

type Stack = [Int]
type Code  = [Op]
data Op    = PUSH Int | ADD deriving (Eq, Show)


-- note uses initial stack to give final stack
exec                 :: Code -> Stack -> Stack
exec [] s            = s
exec (PUSH n:c) s    = exec c (n:s)
exec (ADD:c) (m:n:s) = exec c (n + m:s)

-- note: compiles an expression into code
comp           :: Expr -> Code
comp (Val n)   = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]



main = do
    print $ eval e1
    print $ comp e1
    print $ exec (comp e1) [] -- == [eval e1]



{-
NOTE prove: exec (comp e) s = eval e : s

1. base case:

exec (comp (Val n)) s
= exec [PUSH n] s
= n : s
= eval (Val n):s


2. inductive case:

exec (comp (Add x y)) s
= exec (comp x ++ comp y ++ [ADD]) s
= exec (comp x ++ (comp y ++ [ADD])) s
= exec (comp y ++ [ADD]) (exec (comp x) s)    note distributivity
= exec (comp y ++ [ADD]) (eval x : s)         note induction hypothesis for x
= exec [ADD] (exec (comp y) (eval x:s))      note distributivity again
= exec [ADD] (exec y : eval x : s)         note induction hypothesis for y
= (eval x + eval y) : s
= eval (Add x y) : s




NOTE the distributivity property says :
exec (c ++ d) s = exec d (exec c s)

1. base case

exec ([] ++ d) s
= exec d s
= exec d (exec [] s)


2. inductive case (1)

exec ((PUSH n: c) ++ d) s
= exec (PUSH n: (c ++ d)) s  note applying ++
= exec (c ++ d) (n:s)      note induction hypothesis
= exec d (exec c(n:s))   note unapplying exec
= exec d (exec (PUSH n:c) s)


inductive case (2)

exec ((ADD:c) ++ d) s
= exec (ADD : (c++d)) s
= exec (ADD : (C ++ d)) (m:n:s')
= exec (c++d) (n+m : s')
= exec d (exec c (n+m : s'))
= exec ys (exec (ADD : c) (m:n:s')) note unapplying exec




HELP HELP HELP
NOTE prove
comp' e c = comp e ++ c

comp'             :: Expr -> Code -> Code
comp' (Val n) c   = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))


prove
exec (comp' e c) s = exec c (eval e: s)

1. base case

exec (comp' (Val n) c) s
= exec (PUSH n : c) s
= exec c (n:s)
= exec c (eval (Val n):s)


2. inductive case:

exec (comp (Add x y) c) s
= exec (comp' x (comp' y (ADD : c))) s
= exec (comp' y (ADD : c)) (eval x:s)  note inductive hypothesis for x
= exec (comp' y (ADD : c)) (eval x: s) note induction hypothesis for y help
= exec (ADD : c) (eval y : eval x : s)
= exec c ((eval x + eval y) : s)
= exec c (eval (Add x  y) : s)  note unapplying eval



-}
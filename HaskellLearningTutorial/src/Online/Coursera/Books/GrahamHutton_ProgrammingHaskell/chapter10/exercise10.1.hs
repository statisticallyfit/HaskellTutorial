
data Nat = Zero | Succ Nat deriving (Eq, Show)
-- natural numbers are from 0 -> infinity

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Succ (intToNat (n-1))



add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)


mult            :: Nat -> Nat -> Nat
mult m Zero     = Zero
mult m (Succ n) = add m (mult m n)

-- HELP did not understand how mult works, understand better


data Nat = Zero | Succ Nat deriving (Eq, Show)


-- >> Zero               --> 0
-- >> (Succ Zero)        --> 1
-- >> (Succ (Succ Zero)) --> 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n


-- >> 0  --> Just Zero
-- >> 1  --> Just (Succ Zero)
-- >> 2  --> Just (Succ (Succ Zero))
-- >> -1 --> Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat int
    | int < 0 = Nothing
    | otherwise = Just $ intToNat int
    where intToNat 0 = Zero
          intToNat x = Succ (intToNat (x - 1))
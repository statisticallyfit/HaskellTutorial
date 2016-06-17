import Data.List
import Data.Maybe


data Roman = Roman Integer -- deriving (Eq, Show)




-- method 1 -----------------------------------------------------------------------
convMap = [(1000,"M"), (900,"CM"), (500,"D"), (400,"CD"), (100,"C"),
           (90, "XC"), (50, "L"), (40, "XL"), (10,"X"), (9,"IX"), (5,"V"),
           (4,"IV"), (1,"I")]

toRoman :: Integer -> String
toRoman 0 = "N"
toRoman x = roman x

roman :: Integer -> String
roman x
    | x == 0 = ""
    | x > 0  = b ++ roman (x - a)
        where (a,b) = head $ filter ((<= x) . fst) convMap
        -- note could also write:  filter (\(n,s) -> n <= 48) convMap


-- method 2 -----------------------------------------------------------------------
-- HELP understand evaluation
toRoman' :: Integer -> String
toRoman' 0 = "N"
toRoman' x | x > 0 = snd $ foldl f (x, []) convMap
    where f (n,s) (rn, rs) = (l, s ++ concat (genericReplicate k rs))
                where (k,l) = divMod n rn



-- method 3 -----------------------------------------------------------------------
-- HELP understand evaluation!
toRoman'' :: Integer -> String
toRoman'' = concat . unfoldr findLeast
    where findLeast n = case i of
                            Just (x,r) -> Just (r, n-x)
                            Nothing    -> Nothing
                            where i = find (\(val,_) -> val <= n) convMap
{-
A note on how unfoldr works: you pass it a seed value and a function.
This function returns either Just(a,b) or Nothing. In the former case, a is
added to the accumulator and b is used as the next seed value. This will continue
until the function returns Nothing. At this point the unfoldr is complete.
-}





-- exercise 15 --------------------------------------------------------------------
instance Show Roman where
    show (Roman n) = toRoman n

instance Num Roman where
    --(+) :: Roman -> Roman -> Roman
    (+) (Roman n) (Roman m) = Roman (n + m)

    --(*) :: Roman -> Roman -> Roman
    (*) (Roman n) (Roman m) = Roman (n * m)

    --fromInteger :: Integer -> Roman
    fromInteger i = Roman i

    --abs :: Roman -> Roman
    abs = id

    --signum :: Roman -> Roman
    signum = error "Roman numbers aren't signed"

    --negate :: Roman -> Roman
    negate (Roman i) = Roman (-i) -- useful to have neg numbers internally.





-- exercise 16 ---------------------------------------------------------------------
mapping = [("M",1000),("CM",900),("D",500),("CD",400),("C",100),("XC",90),
           ("L",50),("XL",40),("X",10),("IX",9),("V",5),("IV",4),("I",1)]

-- HELP understand evaluation -- why the rest of the list is thrown away at
-- (num, xs):_
toArabic :: String -> Int
toArabic "" = 0
toArabic str = num + toArabic xs
    where (num, xs):_ = [ (num, drop (length n) str) | (n,num) <- mapping,
                                                        isPrefixOf n str ]

{-

instance Read Roman where
    read romanNumeral = toArabic romanNumeral-}


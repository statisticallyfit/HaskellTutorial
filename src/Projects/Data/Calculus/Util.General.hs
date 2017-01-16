{-# LANGUAGE FlexibleContexts #-}
module Util.General where

import Types
import Encoders
import Main
import Util.Specific




------------------------------------------ General ---------------------------------------

-- chisel (Mul e (F f)) = chisel e .* (F $ fmap chisel f) -- TODO other way to handle this? pluckfunc?
-- ---> NOTE help check again but seems to work fine now (above)
-- postcondition: converts negative pow to positive by changing to div or mul.
chisel :: Expr -> Expr
chisel e
    -- | (length $ splitAS expr) > 1 = error "expr must not have exterior added/subtracted terms"
    | expr' == expr = clean $ makeDivExplicit expr
    | otherwise = chisel expr'
    where
    expr = clean e
    expr' = if (hasDiv expr) then (chiseler (makeDivExplicit expr)) else (chiseler expr)
    chiseler (Num n) = Num n
    chiseler (Var x) = Var x
    chiseler (Frac f) = Frac f
    chiseler (Neg e) = Neg $ chiseler e
    chiseler (F f) = F f  -- TODO functor here to map inside and chisel the function args.

    --- note the (4x + 8x^(-22)) simplification cases (with neg been pushed to outside
    -- for non-nums and pushed inside for nums)
    chiseler (Add a (Pow b (Num p)))
        | p < 0 = Div (chiseler a .* chiseler b .^ Num (-1*p) .+ Num 1) (chiseler b .^ Num (-1*p))
        | otherwise = chiseler a .+ (chiseler b .^ (Num p))
    chiseler (Add a (Mul (Num n) (Pow b (Num p))))
        | p < 0 = Div (chiseler a .* (chiseler b .^ Num (-1*p)) .+ Num n) (chiseler b .^ Num (-1*p))
        | otherwise = chiseler a .+ (Num n .* (chiseler b .^ Num p))
    {-chiseler (Div (Add a (Mul (Num n) (Pow b (Num p)))) e2)
        | p < 0
            = (Div (chiseler a .* (chiseler b .^ Num (-1*p)) .+ Num n)
            (chiseler b .^ Num (-1*p)))  ./  (chiseler e2)
        | otherwise = chiseler a .+ (Num n .* (chiseler b .^ Num p))
-}
    chiseler (Add (Pow b (Num p)) a)
        | p < 0 = Div (chiseler a .* chiseler b .^ Num (-1*p) .+ Num 1) (chiseler b .^ Num (-1*p))
        | otherwise = (chiseler b .^ Num p) .+ chiseler a
    chiseler (Add (Mul (Num n) (Pow b (Num p))) a)
        | p < 0 = Div (chiseler a .* (chiseler b .^ Num (-1*p)) .+ Num n) (chiseler b .^ Num (-1*p))
        | otherwise = (Num n .* (chiseler b .^ Num p)) .+ chiseler a

    chiseler (Sub a (Pow b (Num p)))
        | p < 0 = Div (chiseler a .* chiseler b .^ Num (-1*p) .- Num 1) (chiseler b .^ Num (-1*p))
        | otherwise = chiseler a .- (chiseler b .^ (Num p))
    chiseler (Sub a (Mul (Num n) (Pow b (Num p))))
        | p < 0 = Div (chiseler a .* (chiseler b .^ Num (-1*p)) .- Num n) (chiseler b .^ Num (-1*p))
        | otherwise = chiseler a .- (Num n .* (chiseler b .^ Num p))

    chiseler (Sub (Pow b (Num p)) a)
        | p < 0 = Div (chiseler a .* chiseler b .^ Num (-1*p) .- Num 1) (chiseler b .^ Num (-1*p))
        | otherwise = (chiseler b .^ Num p) .- chiseler a
    chiseler (Sub (Mul (Num n) (Pow b (Num p))) a)
        | p < 0 = Div (chiseler a .* (chiseler b .^ Num (-1*p)) .- Num n) (chiseler b .^ Num (-1*p))
        | otherwise = (Num n .* (chiseler b .^ Num p)) .- chiseler a


    --- note: the ((n/m)/p) simplification cases
    chiseler (Div (Div a b) (Div c d)) = Div (chiseler a .* chiseler d) (chiseler b .* chiseler c)
    chiseler (Div (Div a b) other) = Div (chiseler a) (chiseler b .* chiseler other)
    chiseler (Div other (Div c d)) = Div (chiseler other .* chiseler d) (chiseler c)

    --- note div power simplify (x+1)^3/(x+1) = (x+1)^2
    chiseler (Div p1@(Pow a (Num n)) p2@(Pow b (Num m)))
        | a == b && (not $ isMono p1) && (not $ isMono p2) = Pow (chiseler a) (Num (n-m))
        | otherwise = Div (chiseler p1) (chiseler p2)

    chiseler (Div p1@(Pow a (Num n)) b)
        | a == b && (not $ isMono p1) && (not $ isMono b) = Pow (chiseler a) (Num (n-1))
        | otherwise = Div (chiseler p1) (chiseler b)
    {-chiseler (Div (Add a b) c)
        | isDiv a' && isDiv b'
            = Div (Add (getUpper a') (getUpper b')) (getLower a' .* getLower b' .* c')
        | isDiv a' = Div (Add (getUpper a') b') (getLower a' .* c')
        | isDiv b' = Div (Add a' (getUpper b')) (getLower b' .* c')
        | otherwise = (a' .+ b') ./ c'
        where
        a' = chiseler a
        b' = chiseler b
        c' = chiseler c
    chiseler (Div (Sub a b) c)
        | isDiv a' && isDiv b'
            = Div (Sub (getUpper a') (getUpper b')) (getLower a' .* getLower b' .* c')
        | isDiv a' = Div (Sub (getUpper a') b') (getLower a' .* c')
        | isDiv b' = Div (Sub a' (getUpper b')) (getLower b' .* c')
        | otherwise = (a' .- b') ./ c'
        where
        a' = chiseler a
        b' = chiseler b
        c' = chiseler c -}
    ------ note the pow cases.
    -- note keeping these despite making div explicit  because result is (7 * 1) / pow
    -- instead of (7 / pow) and we get the latter if we use the below  (for mm3')
    chiseler (Mul a (Pow base (Neg (Num n))))
        | n >= 0 = Div (chiseler a) (Pow (chiseler base) (Num n))
        | otherwise = Mul (chiseler a) (Pow (chiseler base) (Num (-1*n)))
    chiseler ((Mul a (Pow base (Num n))))
        | n < 0 = Div (chiseler a) (Pow (chiseler base) (Num (-1*n)))
        | otherwise = (chiseler a) .* (chiseler base) .^ Num n
    chiseler (Div a (Pow base (Neg (Num n))))
        | n >= 0 = Mul (chiseler a) (Pow (chiseler base) (Num n))
        | otherwise = Div (chiseler a) (Pow (chiseler base) (Num (-1*n)))
    chiseler (Div a (Pow base (Num n)))
        | n < 0 = (chiseler a) .* (chiseler base) .^ Num (-1*n)
        | otherwise = Div (chiseler a) (Pow (chiseler base) (Num n))

    chiseler (Pow base (Neg (Num n)))
        | n >= 0 = Num 1 ./ ((chiseler base) .^ Num n)
        | otherwise = (chiseler base) .^ Num (-1*n)
    chiseler (Pow base (Num n))
        | n < 0 = Num 1 ./ ((chiseler base) .^ (Num (-1*n)))
        | otherwise = (chiseler base) .^ (Num n)

    -- note the mul-covered cases (above must is of form (Mul (case here) e)
    chiseler (Mul (Mul a (Pow base (Neg (Num n)))) side)
        | n >= 0 = Div (chiseler a .* chiseler side) (Pow (chiseler base) (Num n))
        | otherwise = (chiseler a) .* (chiseler base) .^ (Num (-1*n)) .* (chiseler side)
    chiseler (Mul (Mul a (Pow base (Num n))) side)
        | n < 0 = Div (chiseler a .* chiseler side) (Pow (chiseler base) (Num (-1*n)))
        | otherwise = (chiseler a) .* (chiseler base) .^ Num n .* (chiseler side)
    chiseler (Mul (Div a (Pow base (Neg (Num n)))) side)
        | n >= 0 = Mul (chiseler a) (Pow (chiseler base) (Num n)) .* (chiseler side)
        | otherwise = Div (chiseler a .* chiseler side) (Pow (chiseler base) (Num (-1*n)))
    chiseler (Mul (Div a (Pow base (Num n))) side)
        | n < 0 = (chiseler a) .* (chiseler base) .^ Num (-1*n) .* (chiseler side)
        | otherwise = Div (chiseler a .* chiseler side) (Pow (chiseler base) (Num n))

    chiseler (Mul (Pow base (Neg (Num n))) side)
        | n >= 0 = (chiseler side) ./ ((chiseler base) .^ Num n)
        | otherwise = (chiseler base) .^ Num (-1*n) .* (chiseler side)
    chiseler (Mul (Pow base (Num n)) side)
        | n < 0 = (chiseler side) ./ ((chiseler base) .^ (Num (-1*n)))
        | otherwise = (chiseler base) .^ (Num n) .* (chiseler side)


    --- note the (a/b + c) = (a + cb)/b cases
    chiseler (Add (Div a b) c) = Div (chiseler a .+ chiseler b .* chiseler c) (chiseler b)
    chiseler (Add c (Div a b)) = Div (chiseler c .* chiseler b .+ chiseler a) (chiseler b)
    chiseler (Sub (Div a b) c) = Div (chiseler a .- chiseler b .* chiseler c) (chiseler b)
    chiseler (Sub c (Div a b)) = Div (chiseler c .* chiseler b .- chiseler a) (chiseler b)


    chiseler (Add e1 e2) = Add (chiseler e1) (chiseler e2)
    chiseler (Sub e1 e2) = Sub (chiseler e1) (chiseler e2)
    chiseler (Mul e1 e2) = Mul (chiseler e1) (chiseler e2)
    chiseler (Div e1 e2) = Div (chiseler e1) (chiseler e2)
    chiseler (Pow e1 e2) = Pow (chiseler e1) (chiseler e2)



-- note puts division expressions on the outside. so (Mul some thing div inside) = > DIv (mul some)
-- precondition: must only deal with glued expressions (div and mul and pow)
-- example it we have Mul ((s) / (t), e) = then returns (Div (s * e, t))
-- example if we have (Mul (s/t, s/t)) then returns the  Div (s*t, s*t)
-- TODO note do we want to simplify here and so use mul-mul cases? if not remvove
makeDivExplicit :: Expr -> Expr
makeDivExplicit expr
    | expr' == expr = expr
    | otherwise = makeDivExplicit expr'
    where
    expr' = explicit expr
    explicit (Num n) = Num n
    explicit (Frac f) = Frac f
    explicit (Var v) = Var v
    explicit (F f) = F f
    explicit (Neg e) = Neg $ explicit e
    explicit (Mul (Div a b) (Div c d)) = Div (a .* b) (c .* d)
    explicit (Mul (Div a b) other) = Div (a .* other) b
    explicit (Mul other (Div c d)) = Div (other .* c) d
    explicit (Mul (Mul a (Div x y)) other) = Div (a .* x .* other) y
    explicit (Mul e1 e2) = Mul (explicit e1) (explicit e2)
    --- TODO note is ok? Took these from here and put them in chisel since these are already
    -- div explicit but just need to be chiseled into simpler form.
    {-explicit (Div (Div a b) (Div c d)) = Div (a .* d) (b .* c)
    explicit (Div (Div a b) other) = Div a (b .* other)
    explicit (Div other (Div c d)) = Div (other .* d) c-}
    explicit (Div e1 e2) = Div (explicit e1) (explicit e2)
    explicit (Pow base expo) = Pow (explicit base) (explicit expo)
    explicit (Add e1 e2) = Add (explicit e1) (explicit e2)
    explicit (Sub e1 e2) = Sub (explicit e1) (explicit e2)

-- TODO  note do we want to simplify here to keep the div-div cases? if not remvove
makeMulExplicit :: Expr -> Expr
makeMulExplicit expr
    | expr' == expr = expr
    | otherwise = makeMulExplicit expr'
    where
    expr' = explicit expr
    explicit (Num n) = Num n
    explicit (Frac f) = Frac f
    explicit (Var v) = Var v
    explicit (F f) = F f
    explicit (Neg e) = Neg $ explicit e
    {-explicit (Mul (Div a b) (Div c d)) = Mul (a ./ b) (d ./ c)
    explicit (Mul (Div a b) other) = Mul (a ./ b) (Num 1 ./ other)
    explicit (Mul other (Div c d)) = Mul (other ./ c) d-}
    explicit (Mul e1 e2) = Mul (explicit e1) (explicit e2)
    explicit (Div (Mul a b) (Mul c d)) = Mul (a ./ c) (b ./ d)
    explicit (Div (Mul a b) other) = Mul (a .* b) (Num 1 ./ other)
    explicit (Div other (Mul c d)) = Mul (other ./ c) (Num 1 ./ d)
    explicit (Div (Div a b) (Div c d)) = Mul (a ./ b) (d ./ c)
    explicit (Div (Div a b) other) = Mul (a ./ b) (Num 1 ./ other)
    explicit (Div other (Div c d)) = Mul (other ./ c) d
    explicit (Div e1 e2) = Div (explicit e1) (explicit e2)
    explicit (Pow base expo) = Pow (explicit base) (explicit expo)
    explicit (Add e1 e2) = Add (explicit e1) (explicit e2)
    explicit (Sub e1 e2) = Sub (explicit e1) (explicit e2)







-- testing negExplicit (distribute (negExplicit e)) = negExplicit e
-- testing distribute (neg (distribute e)) = distribute e
-- TODO fix: x(x+1)^3 does not distribute properly.
-- postcondition:
-- 1) distributes minus signs
-- 2) distributes neg signs
-- 3) distributes (separable) * (separable): (x + 1)^3 replic (x+1)(x+1)(x+1) and is passed
-- to distribute case (Mul a (Mul b c)) where a, b, c are separable and Mul a b  [sep].
distribute :: Expr -> Expr
distribute expr
    | expr' == expr = expr
    | otherwise = distribute expr'
    where
    expr' = dist expr
    dist (Num n) = Num n
    dist (Frac f) = Frac f
    dist (Var v) = Var v
    dist (F f) = F f
    dist (Neg (Num n)) = Num (-n)
    dist (Neg (Frac f)) = Frac (-f)
    --- note the negative distribute cases
    dist (Sub a (Sub b c)) = (dist a .- dist b) .+ (dist c)
    dist (Sub a (Add b c)) = (dist a .- dist b) .- (dist c)
    dist (Add a (Sub b c)) = (dist a .+ dist b) .- (dist c)
    dist (Add a (Add b c)) = (dist a .+ dist b) .+ (dist c)
    --- note the -cos(x)(4) = cos(x)(-4) distribute cases
    {-dist (Mul (Neg a) (Num n)) = dist a .* Num (-n)
    dist (Mul (Num n) (Neg b)) = Num (-n) .* dist b -}
    --- note the (x+1)(x+2) distribute cases
    dist (Mul a b)
        | isSeparable a && isSeparable b = zipperAll as bs
        | isSeparable a = zipperAll as [dist b]
        | isSeparable b = zipperAll [dist a] bs
        | otherwise = dist a .* dist b
        where
        as = splitAS (dist a)
        bs = splitAS (dist b)
        zipper ys x = zipWith Mul (replicate (length ys) x) ys
        zipperAll xs ys = simplify $ rebuildAS $ map simplify $ concatMap (zipper xs) ys
    -- NOTE TODO check if correct, the previous place of this block was after neg frac.
    dist (Neg e)
        | isAdd e = (dist (Neg a)) .- (dist b)
        | isSub e = (dist (Neg a)) .+ (dist b)
        | isMul e = (dist (Neg a)) .* (dist b) --otherwise = Neg (dist e)
        | isDiv e = (dist (Neg a)) ./ (dist b)
        | otherwise = Neg $ dist e
        where (a, b) = (left e, right e)
    dist (Add a b) = Add (dist a) (dist b)
    dist (Sub a b) = Sub (dist a) (dist b)
    -- dist (Mul a b) = Mul (dist a) (dist b)
    dist (Div a b) = Div (dist a) (dist b)
    dist (Pow a b) = Pow (dist a) (dist b)

{-zipper :: [Expr] -> Expr -> [Expr]
zipper ys x = zipWith Mul xs ys
    where xs = replicate (length ys) x

zipperAll :: [Expr] -> [Expr] -> Expr
zipperAll xs ys = rebuildAS $ concatMap (zipper ys) xs-}

clean :: Expr -> Expr
clean expr
    | expr' == expr = expr
    | otherwise = cln expr'
    where
    expr' = cln expr
    fracZero = Frac (Rate 0)
    fracOne = Frac (Rate 1)

    --- note the positioning cases TODO need to add Mul (Mul (Num) (f)) (var or num)
    -- but not necessary after meltpolyfunc hopefully.
    cln (Mul (F f) (Var v)) = Var v .* F f
    cln (Mul (F f) (Neg (Var v))) = Neg (Var v) .* F f
    cln (Mul (Neg (F f)) (Var v)) = Neg (Var v) .* F f
    cln (Mul (Neg (F f)) (Neg (Var v))) = Var v .* F f
    cln (Mul (F f) (Num n)) = Num n .* F f
    cln (Mul (F f) (Neg (Num n))) = Neg (Num n) .* F f
    cln (Mul (Neg (F f)) (Num n)) = Neg (Num n) .* F f
    cln (Mul (Neg (F f)) (Neg (Num n))) = Num n .* F f

    cln (Num n) = Num n
    cln (Frac f) = Frac f
    cln (Var v) = Var v
    cln (F f) = F $ fmap cln f
    cln (Add (Frac (Rate 0)) e2) = cln e2
    cln (Add (Num 0) e2) = cln e2
    cln (Add e1 (Frac (Rate 0))) = cln e1
    cln (Add e1 (Num 0)) = cln e1
    cln (Sub (Frac (Rate 0)) e2) = Neg $ cln e2
    cln (Sub (Num 0) e2) = Neg $ cln e2
    cln (Sub e1 (Frac (Rate 0))) = cln e1
    cln (Sub e1 (Num 0)) = cln e1
    cln (Mul (Frac (Rate 1)) e2) = cln e2
    cln (Mul (Num 1) e2) = cln e2
    cln (Mul e1 (Frac (Rate 1))) = cln e1
    cln (Mul e1 (Num 1)) = cln e1
    cln (Mul (Frac (Rate 0)) e2) = Num 0
    cln (Mul (Num 0) e2) = Num 0
    cln (Mul e1 (Frac (Rate 0))) = Num 0
    cln (Mul e1 (Num 0)) = Num 0
    cln (Div (Num n) (Num m)) = Frac (Rate (n % m))
    cln (Div e1 (Frac (Rate 0))) = error "div by zero"
    cln (Div e1 (Num 0)) = error "div by zero"
    cln (Div (Frac (Rate 0)) e2) = Num 0
    cln (Div (Num 0) e2) = Num 0
    cln (Div e1 (Frac (Rate 1))) = cln e1
    cln (Div e1 (Num 1)) = cln e1
    cln (Pow e1 (Frac (Rate 0))) = Num 1
    cln (Pow e1 (Num 0)) = Num 1
    cln (Pow e1 (Frac (Rate 1))) = cln e1
    cln (Pow e1 (Num 1)) = cln e1 --- TODO do power rules next
    cln (Pow (Frac (Rate 0)) e2) = Num 0
    cln (Pow (Num 0) e2) = Num 0
    cln (Pow (Frac (Rate 1)) e2) = Num 1
    cln (Pow (Num 1) e2) = Num 1

    -- note the neg pusher cases
    cln (Neg (Num n)) = Num (-n)
    cln (Neg (Frac f)) = Frac (-f)
    cln (Neg (Neg e)) = cln e
    cln (Neg e) = Neg $ cln e

    cln (Add e1 e2) = cln e1 .+ cln e2
    cln (Sub e1 e2) = cln e1 .- cln e2
    cln (Mul e1 e2) = cln e1 .* cln e2
    cln (Div e1 e2) = cln e1 ./ cln e2
    cln (Pow e1 e2) = cln e1 .^ cln e2







-- note gets a list of all var names in an expression
-- (except for function args and for pow exponent)
vars :: Expr -> [String]
vars expr = nub $ getVars [] expr
    where
    getVars acc (Var varName) = acc ++ [varName]
    getVars acc (Add e1 e2) = getVars (getVars acc e1) e2
    getVars acc (Sub e1 e2) = getVars (getVars acc e1) e2
    getVars acc (Mul e1 e2) = getVars (getVars acc e1) e2
    getVars acc (Div e1 e2) = getVars (getVars acc e1) e2
    getVars acc (Pow base _) = getVars acc base
    getVars acc (Neg e) = getVars acc e
    getVars acc _ = acc

-- expects chiselled input so that the divs have no more divs in denom and numerator.
genVarSimplify :: Expr -> Expr
genVarSimplify expr = rebuildAS (ms ++ ds)
    where
    es = splitAS expr
    (divs, other) = partition (\e -> isDiv e || isNegDiv e) es
    ms = map varMulSimplify other
    ds = map varDivSimplify divs

-- takes an expression that has only Mul in it and simplifies the different var
-- expressions separately.
-- precondition: expr has already been trhough chisel, distribute, ... so it has no
-- type of arg like (x+1)(3) but instead will be passed the separate parts 3x and 3.
varMulSimplify :: Expr -> Expr
varMulSimplify (F f) = F $ fmap genVarSimplify f
varMulSimplify (Neg e) = Neg $ genVarSimplify e
varMulSimplify (Pow base expo) = Pow (genVarSimplify base) (genVarSimplify expo)
varMulSimplify expr = clean $ rebuild MulOp $ map (clean . simplify) groups'
    where
    groups = groupByVar $ split MulOp expr
    (fs, ns) = (groups !! 0, groups !! 1)
    vs = if (null $ tail groups) then [] else (tail (tail groups))
    fs' = fmap simplify' fs
    groups' = map (rebuild MulOp) ([ns] ++ vs ++ [fs'])

{-groupies = groupByVar $ split MulOp ((splitAS vss) !! 18)
(funcs, nums) = (groupies !! 0, groupies !! 1)
vrs = if (null $ tail groupies) then [] else (tail (tail groupies))
funcs' = fmap simplify' funcs
groupies' = map (rebuild MulOp) ([nums] ++ vrs ++ [funcs'])-}


varDivSimplify :: Expr -> Expr
varDivSimplify (Div upper lower) = Div (simplify' upper) (simplify' lower)


--- note returns groups of single exprs like x^2 or y or a^3 that have the same vars in them.
-- precondition: each element in list es is either (coeff-less monomial, number, or func).
-- postcondition: returns list of lists where first list is functions, next is nums,
-- and next is simplified monomials.
groupByVar :: [Expr] -> [[Expr]]
groupByVar es = [fs] ++ [ns] ++ map (map snd) pairGroups
    where
    (other, vs) = partition (\e -> isNum e || isFrac e || isFunction e) es
    pairs = zip (map getVarMatch vs) vs
    varSorter (v1,e1) (v2,e2) = if (v1 < v2) then LT else if (v1 > v2) then GT else EQ
    pairGroups = groupBy (\(v1,_) (v2,_) -> v1 == v2) (sortBy varSorter pairs)
    (fs, ns) = partition isFunction other


--- note returns true if has only one var name
hasOneVar :: Expr -> Bool
hasOneVar expr = (length $ vars expr) == 1

-- note returns true if the var given is the only one in the expr.
varMatch :: String -> Expr -> Bool
varMatch var expr = hasOneVar expr && ((head (vars expr)) == var)

--- note get var that matches to the single one in the expr, like x^2 => "x"
-- precondition: assume each expr is single (has no mul, div,pow,add,or sub). And that
-- it has only one var, not more than one.
getVarMatch :: Expr -> String
getVarMatch expr = head $ filter ((flip varMatch) expr) variables
    where variables = vars expr


put :: Int -> a -> [a] -> [a]
put _ n [] = [n]
put index n xs
    | index < 0 = error "index is negative "
    | otherwise = front ++ [n] ++ (tail back)
    where (front, back) = splitAt index xs
          newBack = if null back then [] else (tail back)

elongate :: Int -> a -> [a] -> [a]
elongate len item xs = xs ++ (replicate (len - (length xs)) item)


toCommonDenom :: ((Int,Int), (Int,Int)) -> ((Int,Int), (Int,Int))
toCommonDenom ((n1,d1), (n2,d2)) = ((n1', lcm), (n2', lcm))
    where
    lcm = leastCommonMultiple d1 d2
    n1' = n1 * (lcm `div` d1)
    n2' = n2 * (lcm `div` d2)


-- note first arg doesn't have to be maybe just made it so that we can use it with foldl.
leastCommonMultiple :: Int -> Int -> Int
leastCommonMultiple a b = lcm a' b' c
    where
    (a', b') = (abs a, abs b)
    c = a' * b'
    lcm a b c
        | not (a == b) = if (a > b) then (lcm (a-b) b c) else (lcm a (b-a) c)
        | otherwise = fromInteger $ numerator $ toRational $ abs (c `div` a) -- convert to get int type




splitAll :: Expr -> [Expr]
splitAll expr = (concatMap (split DivOp) divs) ++ muls
    where (divs, muls) = partition (\e -> isDiv e || isNegDiv e) (splitAS expr)


splitAS :: Expr -> [Expr]
splitAS expr = concatMap (split SubOp) (split AddOp expr)

-- note must be used wisely or else order of operations won't be used.
-- for example:
-- split MulOp e5   ==>    [-(4),-x,-(2),-y] which is not correct
-- but we can do instead:
-- split SubOp e5   ==>    [-4x,-2y]
-- precondition: must take output from distribute (doesn't work on unflattened or unstacked exprs)
split :: Op -> Expr -> [Expr]
split _ (Var x) = [Var x]
split _ (Num n) = [Num n]
split _ (Frac f) = [Frac f]
split _ (F f) = [F f]
split _ p@(Pow _ _) = [p]
split op (Neg e) = handleNeg op (Neg e)
split op expr = pickMethod op expr

splitA :: Expr -> [Expr]
splitA e
    | isDiv e || isMul e || isPow e = [e] -- TODO it's a mess here .. fix some other way.
    | isAdd e = splitAdd e
    | hasAdd e = init lefties ++ (newRight e lefties)
    | isNeg e = handleNeg AddOp e
    | not $ isVarNumFunc e = [e]
    | otherwise = split AddOp e
    where lefties = splitA (left e)
splitAdd (Add e1 e2)
    | notAdd e1 && notAdd e2 = [e1, e2]
    | notAdd e1 = [e1] ++ splitA e2
    | notAdd e2 = splitA e1 ++ [e2]
    | otherwise = splitA e1 ++ splitA e2
    where notAdd = not . hasAdd

splitS :: Expr -> [Expr]
splitS e
    | isDiv e || isMul e || isPow e = [e]
    | isSub e = splitSub e
    | hasSub e = init lefties ++ (newRight e lefties)
    | isNeg e = handleNeg SubOp e
    | not $ isVarNumFunc e = [e]
    | otherwise = split SubOp e
    where lefties = splitS (left e)
splitSub (Sub e1 e2)
    | notSub e1 && notSub e2 = [e1, Neg e2]
    | notSub e1 = [e1] ++ splitS (Neg e2)
    | notSub e2 = splitS e1 ++ [Neg e2]
    | otherwise = splitS e1 ++ map Neg (splitS e2)
    where notSub = not . hasSub

-- TODO -- think of how to splitM e11
-- TODO fix e12 tomorrow
splitM :: Expr -> [Expr]
splitM e
    | isPow e = [e]
    | isMul e = splitMul e
    | isSeparable e = [e] -- note order of operations
    | isDiv e && hasMul e = if (not $ isMul expl) then [e] else (split MulOp expl)
    | hasMul e = init lefties ++ (newRight e lefties)
    | isNeg e = handleNeg MulOp e
    | not $ isVarNumFunc e = [e]
    | otherwise = split MulOp e
    where lefties = splitM (left e)
          expl = makeMulExplicit e
splitMul (Mul e1 e2)
    | notMul e1 && notMul e2 = [e1, e2]
    | notMul e1 = [e1] ++ splitM e2
    | notMul e2 = splitM e1 ++ [e2]
    | otherwise = splitM e1 ++ splitM e2
    where notMul = not . hasMul

-- NOTE no need to fix e9 so that it divides everywhere no?
splitD :: Expr -> [Expr]
splitD e
    | isPow e = [e]
    | isDiv e = splitDiv e
    | isSeparable e = [e] -- note order of operations
    | isMul e && hasDiv e = if (not $ isDiv expl) then [expl] else (split DivOp expl)
    | hasDiv e = init lefties ++ (newRight e lefties)
    | isNeg e = handleNeg DivOp e
    | not $ isVarNumFunc e = [e]
    | otherwise = split DivOp e
    where lefties = splitD (left e)
          expl = makeDivExplicit e
splitDiv (Div e1 e2)
    | notDiv e1 && notDiv e2 = [e1, e2]
    | notDiv e1 = [e1] ++ splitD e2
    | notDiv e2 = splitD e1 ++ [e2]
    | otherwise = splitD e1 ++ splitD e2
    where notDiv = not . hasDiv



pickMethod :: Op -> Expr -> [Expr]
pickMethod AddOp expr = splitA expr
pickMethod SubOp expr = splitS expr
pickMethod MulOp expr = splitM {-$ makeMulExplicit-} expr
pickMethod DivOp expr = splitD {-$ makeDivExplicit-} expr

handleNeg :: Op -> Expr -> [Expr]
handleNeg op (Neg e)
    | isAdd e && (op == SubOp) = map Neg (pickMethod AddOp e)
    | isAdd e || isSub e = map Neg pick
    | isMul e || isDiv e || isPow e = [Neg (head pick)] ++ tail pick
    | otherwise = [Neg (head vnf)] ++ tail vnf
    where pick = pickMethod op e
          vnf = split op e -- stands for var or num or func

newRight :: Expr -> [Expr] -> [Expr]
newRight e ls
    | isAdd e = [(last ls) .+ foldl1 Add (split AddOp (right e))]
    | isSub e = [(last ls) .- foldl1 Sub (split SubOp (right e))]
    | isMul e = [(last ls) .* foldl1 Mul (split MulOp (right e))]
    | isDiv e = [(last ls) ./ foldl1 Div (split DivOp (right e))]
    | isPow e = [(last ls) .^ foldl1 Pow (split PowOp (right e))]
    | otherwise = [e]



-- rebuilds with add sub signs.
rebuildAS :: [Expr] -> Expr
rebuildAS es = clean $ foldl f (Num 0) es
    where
    f acc x
        | (not (acc == (Num 0))) && isNeg x = (Sub acc (getNeg x))
        | otherwise = Add acc x

rebuild :: Op -> [Expr] -> Expr
rebuild AddOp es = clean $ foldl (\acc x -> Add acc x) (Num 0) es
rebuild SubOp es = clean $ foldl (\acc x -> Sub acc x) (Num 0) es
rebuild MulOp es = clean $ foldl (\acc x -> Mul acc x) (Num 1) es
rebuild DivOp es = clean $ foldl (\acc x -> Div acc x) (Num 1) es
rebuild PowOp es = clean $ foldl (\acc x -> Pow acc x) (Num 1) es




isNegDiv :: Expr -> Bool
isNegDiv e = if isNeg e then (isDiv (getNeg e)) else False






-- brings the neg out to the front
negExplicit :: Expr -> Expr
negExplicit expr
    | expr' == expr = expr
    | otherwise = negExplicit expr'
    where
    expr' = neg expr
    neg e@(Num n) = if n < 0 then (Neg (Num (-1*n))) else e
    neg e@(Frac (Rate n)) = if n < 0 then (Neg (Frac (Rate (-1*n)))) else e
    neg e@(F f) = e
    neg e@(Var x) = e
    neg (Neg (Neg n)) = neg n
    neg (Neg n) = Neg $ neg n

    -- Note tehse are removed from clean() and put here temporarily.
    neg (Add (Neg a) (Neg b)) = Neg $ neg a .+ neg b
    neg (Add a (Neg b)) = neg a .- neg b
    neg (Add (Neg a) b) = Neg $ neg a .- neg b
    neg (Sub (Neg a) (Neg b)) = Neg $ neg a .- neg b
    neg (Sub a (Neg b)) = neg a .+ neg b
    neg (Sub (Neg a) b) = Neg $ neg a .+ neg b
    -- note like above from clean and put here.
    neg (Mul (Neg a) (Neg b)) = neg a .* neg b
    neg (Mul (Neg a) b) = Neg $ neg a .* neg b
    neg (Mul a (Neg b)) = Neg $ neg a .* neg b
    neg (Div (Neg a) (Neg b)) = neg a ./ neg b
    neg (Div (Neg a) b) = Neg $ neg a ./ neg b
    neg (Div a (Neg b)) = Neg $ neg a ./ neg b

    neg e@(Add a b) = Add (neg a) (neg b)
    neg e@(Sub a b) = Sub (neg a) (neg b)
    neg e@(Mul a b) = Mul (neg a) (neg b)
    neg e@(Div a b) = Div (neg a) (neg b)
    neg e@(Pow a b) = Pow (neg a) (neg b)




------------------------

isAdd :: Expr -> Bool
isAdd (Add _ _) = True
isAdd _ = False

isSub :: Expr -> Bool
isSub (Sub _ _) = True
isSub _ = False

isMul :: Expr -> Bool
isMul (Mul _ _) = True
isMul _ = False

isDiv :: Expr -> Bool
isDiv (Div _ _) = True
isDiv _ = False

getUpper :: Expr -> Expr
getUpper (Div numer _) = numer
getUpper (Neg (Div numer _)) = Neg numer
getUpper _ = error "no div expr in getUpper"

getLower :: Expr -> Expr
getLower (Div _ denom) = denom
getLower (Neg (Div _ denom)) = Neg $ denom
getLower _ = error "no div expr in getLower"

isPow :: Expr -> Bool
isPow (Pow _ _) = True
isPow _ = False

getPow :: Expr -> Expr
getPow (Pow _ expo) = expo
getPow (Neg (Pow _ expo)) = expo
getPow expr = Num 1 -- if not an actual power, then expo is 1

getBase :: Expr -> Expr
getBase (Pow base _) = base
getBase (Neg (Pow base _)) = base
getBase expr = expr

isNum :: Expr -> Bool
isNum (Num _) = True
isNum (Neg (Num _)) = True
isNum _ = False

getNum :: Expr -> Int
getNum (Num n) = n
getNum (Neg (Num n)) = -n

isFrac :: Expr -> Bool
isFrac (Frac _) = True
isFrac (Neg (Frac _)) = True
isFrac _ = False

getFrac :: Expr -> Fraction
getFrac (Frac f) = f
getFrac (Neg (Frac f)) = -f

getNumOrFrac :: Expr -> Fraction
getNumOrFrac expr
    | isNum expr  = Rate $ (getNum expr) % 1
    | isFrac expr = getFrac expr

isNegNumOrFrac :: Expr -> Bool
isNegNumOrFrac (Neg (Num _)) = True
isNegNumOrFrac (Neg (Frac _)) = True
isNegNumOrFrac _ = False

isNumOrFracNeg :: Expr -> Bool
isNumOrFracNeg (Num n) = n < 0
isNumOrFracNeg (Frac f) = f < 0
isNumOrFracNeg _ = False

isVar :: Expr -> Bool
isVar (Var _) = True
isVar _ = False

isNeg :: Expr -> Bool
isNeg (Neg e) = True
isNeg _ = False

getNeg :: Expr -> Expr
getNeg (Neg n) = n
getNeg _ = error "incorrect argument"


--- Trigonometric ---
isSin :: Expr -> Bool
isSin (F (Sin _)) = True
isSin _ = False

isCos :: Expr -> Bool
isCos (F (Cos _)) = True
isCos _ = False

isTan :: Expr -> Bool
isTan (F (Tan _)) = True
isTan _ = False

isCsc :: Expr -> Bool
isCsc (F (Csc _)) = True
isCsc _ = False

isSec :: Expr -> Bool
isSec (F (Sec _)) = True
isSec _ = False

isCot :: Expr -> Bool
isCot (F (Cot _)) = True
isCot _ = False

isArcsin :: Expr -> Bool
isArcsin (F (Arcsin _)) = True
isArcsin _ = False

isArccos :: Expr -> Bool
isArccos (F (Arccos _)) = True
isArccos _ = False

isArctan :: Expr -> Bool
isArctan (F (Arctan _)) = True
isArctan _ = False

isArccsc :: Expr -> Bool
isArccsc (F (Arccsc _)) = True
isArccsc _ = False

isArcsec :: Expr -> Bool
isArcsec (F (Arcsec _)) = True
isArcsec _ = False

isArccot :: Expr -> Bool
isArccot (F (Arccot _)) = True
isArccot _ = False

--- Hyperbolic ---

isSinh :: Expr -> Bool
isSinh (F (Sinh _)) = True
isSinh _ = False

isCosh :: Expr -> Bool
isCosh (F (Cosh _)) = True
isCosh _ = False

isTanh :: Expr -> Bool
isTanh (F (Tanh _)) = True
isTanh _ = False

isCsch :: Expr -> Bool
isCsch (F (Csch _)) = True
isCsch _ = False

isSech :: Expr -> Bool
isSech (F (Sech _)) = True
isSech _ = False

isCoth :: Expr -> Bool
isCoth (F (Coth _)) = True
isCoth _ = False

isArcsinh :: Expr -> Bool
isArcsinh (F (Arcsinh _)) = True
isArcsinh _ = False

isArccosh :: Expr -> Bool
isArccosh (F (Arccosh _)) = True
isArccosh _ = False

isArctanh :: Expr -> Bool
isArctanh (F (Arctanh _)) = True
isArctanh _ = False

isArccsch :: Expr -> Bool
isArccsch (F (Arccsch _)) = True
isArccsch _ = False

isArcsech :: Expr -> Bool
isArcsech (F (Arcsech _)) = True
isArcsech _ = False

isArccoth :: Expr -> Bool
isArccoth (F (Arccoth _)) = True
isArccoth _ = False

--- Logarithmic ---

isLn :: Expr -> Bool
isLn (F (Ln _)) = True
isLn _ = False

isLog :: Expr -> Bool
isLog (F (Log _ _)) = True
isLog _ = False

isE :: Expr -> Bool
isE (F (E _)) = True
isE _ = False

--- Function families ---
isSinFamily :: Expr -> Bool
isSinFamily e = isSin e || isSinh e || isArcsin e || isArcsinh e

isCosFamily :: Expr -> Bool
isCosFamily e = isCos e || isCosh e || isArccos e || isArccosh e

isTanFamily :: Expr -> Bool
isTanFamily e = isTan e || isTanh e || isArctan e || isArctanh e

isCscFamily :: Expr -> Bool
isCscFamily e = isCsc e || isCsch e || isArccsc e || isArccsch e

isSecFamily :: Expr -> Bool
isSecFamily e = isSec e || isSech e || isArcsec e || isArcsech e

isCotFamily :: Expr -> Bool
isCotFamily e = isCot e || isCoth e || isArccot e || isArccoth e

isLogFamily :: Expr -> Bool
isLogFamily e = isE e || isLn e || isLog e

---------------------------------
-- does not refer to data Op, just data Expr (Add, sub, mul..)
isOp :: Expr -> Bool
isOp e = isAdd e || isSub e || isMul e || isDiv e || isPow e



-- returns true if exponential function (fixed base raised to variable power: a^x)
isExponential :: Expr -> Bool
isExponential (Neg e) = isExponential e
isExponential (Pow base expo) = isConstant base && isVariable expo
isExponential _ = False

isConstant :: Expr -> Bool
isConstant (Neg e) = isConstant e
isConstant (Frac f) = True
isConstant (Num n) = True
isConstant _ = False

isVariable :: Expr -> Bool
isVariable = not . isConstant


isVarNumFunc :: Expr -> Bool
isVarNumFunc (Var _) = True
isVarNumFunc (Num _) = True
isVarNumFunc (F f) = True
isVarNumFunc _ = False



isTrig :: Expr -> Bool
isTrig f = isSin f || isCos f || isTan f || isCsc f || isSec f || isCot f

isInvTrig :: Expr -> Bool
isInvTrig f = isArcsin f || isArccos f || isArctan f
    || isArccsc f || isArcsec f || isArccot f

isHyp :: Expr -> Bool
isHyp f = isSinh f || isCosh f || isTanh f || isCsch f || isSech f || isCoth f

isInvHyp :: Expr -> Bool
isInvHyp f = isArcsinh f || isArccosh f || isArctanh f
    || isArccsch f || isArcsech f || isArccoth f

isLogar :: Expr -> Bool
isLogar f = isLog f || isE f || isLn f

isFunction :: Expr -> Bool
isFunction (F _) = True
isFunction (Neg (F _)) = True
isFunction _ = False

-- puts an expression inside a function
push :: Expr -> Expr -> Expr
push newExpr (F f) = F $ fmap (\oldExpr -> newExpr) f
psuh _ e = e



hasAdd :: Expr -> Bool
hasAdd (Var _) = False
hasAdd (Add _ _) = True
hasAdd (Num _) = False
hasAdd (Frac _) = False
hasAdd (Neg e) = hasAdd e
hasAdd (F f) = False
hasAdd (Sub e1 e2) = hasAdd e1 || hasAdd e2
hasAdd (Mul e1 e2) = hasAdd e1 || hasAdd e2
hasAdd (Div e1 e2) = hasAdd e1 || hasAdd e2
hasAdd (Pow e1 e2) = hasAdd e1 || hasAdd e2

hasSub :: Expr -> Bool
hasSub (Var _) = False
hasSub (Sub _ _) = True
hasSub (Num _) = False
hasSub (Frac _) = False
hasSub (Neg e) = hasSub e
hasSub (F f) = False
hasSub (Add e1 e2) = hasSub e1 || hasSub e2
hasSub (Mul e1 e2) = hasSub e1 || hasSub e2
hasSub (Div e1 e2) = hasSub e1 || hasSub e2
hasSub (Pow e1 e2) = hasSub e1 || hasSub e2

hasMul :: Expr -> Bool
hasMul (Var _) = False
hasMul (Mul _ _) = True
hasMul (Num _) = False
hasMul (Frac _) = False
hasMul (Neg e) = hasMul e
hasMul (F f) = False
hasMul (Add e1 e2) = hasMul e1 || hasMul e2
hasMul (Sub e1 e2) = hasMul e1 || hasMul e2
hasMul (Div e1 e2) = hasMul e1 || hasMul e2
hasMul (Pow e1 e2) = hasMul e1 || hasMul e2

hasDiv :: Expr -> Bool
hasDiv (Var _) = False
hasDiv (Div _ _) = True
hasDiv (Num _) = False
hasDiv (Frac _) = False
hasDiv (Neg e) = hasDiv e
hasDiv (F f) = False
hasDiv (Add e1 e2) = hasDiv e1 || hasDiv e2
hasDiv (Sub e1 e2) = hasDiv e1 || hasDiv e2
hasDiv (Mul e1 e2) = hasDiv e1 || hasDiv e2
hasDiv (Pow e1 e2) = hasDiv e1 || hasDiv e2

hasFunction :: Expr -> Bool
hasFunction (Var _) = False
hasFunction (F _) = True
hasFunction (Num _) = False
hasFunction (Frac _) = False
hasFunction (Neg e) = hasFunction e
hasFunction (Add e1 e2) = hasFunction e1 || hasFunction e2
hasFunction (Sub e1 e2) = hasFunction e1 || hasFunction e2
hasFunction (Mul e1 e2) = hasFunction e1 || hasFunction e2
hasFunction (Div e1 e2) = hasFunction e1 || hasFunction e2
hasFunction (Pow e1 e2) = hasFunction e1 || hasFunction e2


hasNeg :: Expr -> Bool
hasNeg (Var _) = False
hasNeg (F _) = False
hasNeg (Num n) = n < 0
hasNeg (Frac f) = f < 0
hasNeg (Neg _) = True
hasNeg (Add e1 e2) = hasNeg e1 || hasNeg e2
hasNeg (Sub e1 e2) = hasNeg e1 || hasFunction e2
hasNeg (Mul e1 e2) = hasNeg e1 || hasNeg e2
hasNeg (Div e1 e2) = hasNeg e1 || hasNeg e2
hasNeg (Pow e1 e2) = hasNeg e1 || hasNeg e2


hasNegPow :: Expr -> Bool
hasNegPow (Var _) = False
hasNegPow (F _) = False
hasNegPow (Num _) = False
hasNegPow (Frac _) = False
hasNegPow (Pow _ (Neg (Num n))) = n > 0
hasNegPow (Pow _ (Num n)) = n > 0
hasNegPow (Pow _ (Neg (Frac f))) = f > 0
hasNegPow (Pow _ (Frac f)) = f > 0
hasNegPow (Neg e) = hasNegPow e
hasNegPow (Add e1 e2) = hasNegPow e1 || hasNegPow e2
hasNegPow (Sub e1 e2) = hasNegPow e1 || hasNegPow e2
hasNegPow (Mul e1 e2) = hasNegPow e1 || hasNegPow e2
hasNegPow (Div e1 e2) = hasNegPow e1 || hasNegPow e2
hasNegPow (Pow e1 e2) = hasNegPow e1 || hasNegPow e2



-- counts total number of separate terms, where powers are counted as 1 term.
numTerms :: Expr -> Int
numTerms expr = length $ divid
    where
    added = split AddOp expr
    subbed = concatMap (split SubOp) added
    multip = concatMap (split MulOp) subbed
    divid = concatMap (split DivOp) multip


left :: Expr -> Expr
left (Var v) = Var v
left (Num n) = Num n
left (Frac f) = Frac f
left (F f) = F f
left (Neg e) = Neg (left e)
left (Add e1 e2) = e1
left (Sub e1 e2) = e1
left (Mul e1 e2) = e1
left (Div e1 e2) = e1
left (Pow e1 e2) = e1


right :: Expr -> Expr
right (Var v) = Var v
right (Num n) = Num n
right (Frac f) = Frac f
right (F f) = F f
right (Neg e) = Neg (right e)
right (Add e1 e2) = e2
right (Sub e1 e2) = e2
right (Mul e1 e2) = e2
right (Div e1 e2) = e2
right (Pow e1 e2) = e2



notZero :: Expr -> Bool
notZero (Num 0) = False
notZero _ = True






-- note this is passed only glued expressions!
-- example 3x^7x^2sin(4x) = True
-- example (sin (sin x)) = True
-- example (sinxtanx) = False
hasOnlyOneFunction :: Expr -> Bool
hasOnlyOneFunction expr = (countFunc 0 expr) == 1

hasManyFunctions :: Expr -> Bool
hasManyFunctions expr = (countFunc 0 expr) > 1

-- note doesn't matter if there is func in expo of a power because even if there is we can
-- put it in description as a simple expo (expos have have any type of expr)
countFunc :: Int -> Expr -> Int
countFunc c (Num n) = c
countFunc c (Var v) = c
countFunc c (Frac f) = c
countFunc c (F f) = c + 1
countFunc c (Neg e) = countFunc c e
countFunc c (Add e1 e2) = countFunc c e1 + countFunc c e2
countFunc c (Sub e1 e2) = countFunc c e1 + countFunc c e2
countFunc c (Mul e1 e2) = countFunc c e1 + countFunc c e2
countFunc c (Div e1 e2) = countFunc c e1 + countFunc c e2
countFunc c (Pow e1 e2) = countFunc c e1 -- + oneFunc count e2

-- note do not count isItem in the exponent of a power because there it doesn't matter as we
-- use the expo in descrip.
-- HELP todo not working ebcause it counts Ops way too many times.
{-count :: (Expr -> Bool) -> Expr -> Int
count f expr = counter 0 f expr
    where
    newC c = if (f x) then (c + 1) else c
    counter c f x@(Num _) = newC c
    counter c f x@(Frac _) = newC c
    counter c f x@(Var _) = newC c
    counter c f x@(F _) = newC c
    counter c f x@(Neg e) = counter (newC c) f e
    counter c f x@(Add e1 e2) = counter (newC c) f e1 + counter (newC c) f e2
    counter c f x@(Sub e1 e2) = counter (newC c) f e1 + counter (newC c) f e2
    counter c f x@(Mul e1 e2) = counter (newC c) f e1 + counter (newC c) f e2
    counter c f x@(Div e1 e2) = counter (newC c) f e1 + counter (newC c) f e2
    counter c f x@(Pow e1 e2) = counter (newC c) f e1 -- + count c' isItem e2-}


{-
count c isItem n@(Num _) = c'
    where c' = if (isItem n) then (c + 1) else c
count c isItem f@(Frac _) = c'
    where c' = if (isItem f) then (c + 1) else c
count c isItem v@(Var _) = c'
    where c' = if (isItem v) then (c + 1) else c
count c isItem f@(F _) = c'
    where c' = if (isItem f) then (c + 1) else c
count c isItem n@(Neg e) = count c' isItem e
    where c' = if (isItem n) then (c + 1) else c

count c isItem a@(Add e1 e2) = count c' isItem e1 + count c' isItem e2
    where c' = if (isItem a) then (c + 1) else c
count c isItem s@(Sub e1 e2) = count c' isItem e1 + count c' isItem e2
    where c' = if (isItem s) then (c + 1) else c
count c isItem m@(Mul e1 e2) = count c' isItem e1 + count c' isItem e2
    where c' = if (isItem m) then (c + 1) else c
count c isItem d@(Div e1 e2) = count c' isItem e1 + count c' isItem e2
    where c' = if (isItem d) then (c + 1) else c
count c isItem p@(Pow e1 e2) = count c' isItem e1 -- + count c' isItem e2
    where c' = if (isItem p) then (c + 1) else c
-}




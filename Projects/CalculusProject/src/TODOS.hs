module TODOS where 

{-
TODO PLAN OVERALL * ~ *

input -> split Add and Sub -> partition -> (poly terms, functions and polyfuncs)

poly terms -> send to addpoly

functions -> partition
	-> type 1: x^2 sin(x) - send to addSingleFunctions
	-> type 2: sinxtanxcosx - send to function simplifyFunctions (or fold using
	simplifyFunctions principles)
type 1:
principle:
represent sin ^ tan x (x) or sin^ 2 (3x^6) as:
Trig [(tan x, X), 0,0,0,0,0] and,
Trig [(Num 2, (3x^6)),0,0,0,0,0] respectively.

principle: we add only if both elements in tuple are equal pairwise, (same for sub) resulting in
new data holder: Trig [(Num 2, same exp, same arg)]

and we multiply only if the argument is equal (snd) so we get Trig [(tanx + Num 2 as exp, X)]
assuming arg was X for both. same for div

---

Clean up with sweep Num
ALSO: order: distribute, negExplicit, then clean the simplified expr and input to simplify function
order for melt polyfunc (so we dont end up with -(16 x + 16x) when in fact we need (-16x + 16x)
=> negExplicit then distribute then clean.


** Also make function called rearrange or organize that transforms:
1 * sin x * cos x * 2 * 4 * ln x * 8 * (-x)
into
-64x * sinx * cos x * ln x

By:
assigning priorities to things:
num/negnum/numneg = priority 0 (highest)
var = priority 1
isMono = priority 2
(x + 1) ^ 2 or (x + 1) ( things like monomials that are nto quite) = priority 3
function = priority 4 (last)

-- precondition of function rearrange is: glued expr, so no add or sub (only as inner
like in x(x+1)(9)). Method: use chisel to get Div separate if present.

------------------------- TODOS ------------------------
* fix distribute to work with x(x+1)^3
* make no decimals - all the calculations and nums will be fractions (2, 3)
-}




-- TODO fix simplify errors:
-- precondition: *** single variable *** simplification.
-- errors: TODO
-- 1) figure out how to combine the simplifymultivar with simplifysinglevar to make single simplify.
-- 2) represent RootPolys where pow = frac 1/3.
-- 3) handle each div
-- 4) handle non divs that need to be simplified: (x + 1)^3 (factor out when expo
-- is no bigger than 5, so 5 and under. If greater leave it as is.
-- 5) START HERE TOMORROW TODO: make polyroot instance of code and make all current
-- poly functions deal only with Nums not fractions, and polyroot to deal with fractions.
-- 6) if result has only polynomials or powers then return the highest polynomials first
-- then nums then powers.
-- 7) make sure to simplify powers correctly for e27 type exprs.




--- TODO fix divSimplify
-- note expecting the remains from other'' in above function (must be div)
-- precondition: get one non-separable div expr at a time.
-- HELP goes into infinite loop ebcause of simplify at the front - FIX TODO




-- TODO make polynomials and trigs and other functions be raised to powers of functions
-- not just constants (make the middle element in tuple to be an expression)
-- While you're at it, make the first coeff an expression so that we can do xsin(x) + 2xsin(x)
-- TODO return error if one of the tuples has patterns (1, 0, ..) because that yields a constant.
-- NOTE rules:
-- Poly: [1, 0, 2, 7, -4] represents 1 + x^2 + 7x^3 - 4x^4
-- Trig: [(0,1), (1,1), (4,1), (1,1), (2,4), (3, 5)]
--      represents (cos x + 4tan x + csc x + 2sec^4 x  + 3cot^5 x
--      where x = any expression and note (0, 1) = 0 always and (1,0) gets error
-- help TODO fix so that it handles (expr, expr, expr) and not (int, int, expr)
{-
groupToExpr :: Group -> [Expr]
groupToExpr (Poly ps) = reverse $ map simplify $ zipWith (.*) ps' (zipWith (.^) xs es )
    where xs = replicate (length ps) X
          es = map Num [0 .. (length ps - 1)]
          ps' = map Num ps
-- TODO inside function is holder -- find way to deal with that.
groupToExpr (Trig ts) = map simplify $ latch (zip ts fs)
    where fs = map F [Sin X, Cos X, Tan X, Csc X, Sec X, Cot X]
groupToExpr (InvTrig ts) = map simplify $ latch (zip ts fs)
    where fs = map F [Arcsin X, Arccos X, Arctan X, Arccsc X, Arcsec X, Arccot X]
groupToExpr (Hyperbolic hs) = map simplify $ latch (zip hs fs)
    where fs = map F [Sinh X, Cosh X, Tanh X, Csch X, Sech X, Coth X]
groupToExpr (InvHyp hs) = map simplify $ latch  (zip hs fs)
    where fs = map F [Arcsinh X, Arccosh X, Arctanh X, Arccsch X, Arcsech X, Arccoth X]
groupToExpr (Logarithmic ls) = map simplify $ latch (zip ls fs)
    where fs = map F [E X, Ln X, Log X X]  -- TODO fix so we can have different args for log base and arg.
-}



-- START HERE tomorrow isMono: was working on codifying but first update sweep to save mono
-- IDEA IMPORTANT: make sweep so that it goes into (Add e1 e2) = Add (sweep e1) (sweep e2)
-- and then deal with gathering up constants for glued expressions.


--- TODO make types Monomial and Polynomial

-- TODO check chisel, distribute, negExplicit workings, clean, makeMulExp, makeDivExplicit



-- testing negExplicit (distribute (negExplicit e)) = negExplicit e
-- testing distribute (neg (distribute e)) = distribute e
-- TODO fix: x(x+1)^3 does not distribute properly.
-- postcondition:
-- 1) distributes minus signs
-- 2) distributes neg signs
-- 3) distributes (separable) * (separable): (x + 1)^3 replic (x+1)(x+1)(x+1) and is passed
-- to distribute case (Mul a (Mul b c)) where a, b, c are separable and Mul a b  [sep].


-- TODO
-- genius:
-- foldl1 (\acc y -> if isNum y then (acc .* y) else acc)    (unGlue e1)

-- note passed a thing like 4xsinxcosx4x^2tanxsecx(3)(2) and result is: 96sinxcosx(x^2)tanxsecx
-- where we write sweep isNum arg to get result.
-- precondition: the expr has to be glued! No add or subtract, just multiply and power.
-- NOTE todo does not work if there are divisions... we just multiply. (look at unglue todo)
-- IDEA or maybe we just use for this multiple trig functions and simplifying general x^2sinx functions
-- before codifying them in codifyOther
-- help don't know whether to simplify expr first or to pass to sweep.





-- TODO idea: count num elements and then decide whether ot put brackets.
-- Example: x^6 * 4 is shown as 4x^6 while 4 * (x+3) is shown as 4(x+3)
-- idea: glued things are wrapped each. for SHow Expr

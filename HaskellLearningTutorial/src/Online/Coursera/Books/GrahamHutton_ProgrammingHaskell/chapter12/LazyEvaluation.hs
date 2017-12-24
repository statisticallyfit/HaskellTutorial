{-
NOTE with lazy evaluation, end result may be different.

IDEA An expression that has the form of a function applied to one or more arguments
that can be “reduced” by performing the application is called a reducible
expression, or redex for short. As indicated by the use of quotations marks in
the preceding sentence, such reductions do not necessarily decrease the size of
an expression, although in practice this is often the case.

IDEA Evaluation: choose the innermost redex to solve -----------------------------

1. Innermost evaluation strategy (call by value)

When evaluating an expression, in what order should reductions be performed?
One common strategy, called innermost evaluation, is to always choose
a redex that is innermost, in the sense that it contains no other redex. If there is
more than one innermost redex, by convention we choose that which begins at
the leftmost position in the expression.
note args are then passed by value.

example
mult (1 + 2, 2 + 3)
={applying the ﬁrst +}
mult (3, 2 + 3)
={applying +}
mult (3, 5)
={applying mult }
3 ∗ 5
={applying ∗}
15


2. Outermost evaluation strategy (call by name)

First evaluate the outermost redex (outermost because it's not containing in
any other redex).
note: args are then passed by name.

example
mult (1 + 2, 2 + 3)
={applying
mult }
(1 + 2) ∗ (2 + 3)
={applying the ﬁrst +}
3 ∗ (2 + 3)
={applying +}
3 ∗ 5
={applying ∗}
15







NOTE: not allowed for lambdas to be reduced so
\x -> 1 + 2
will stay unreduced until it is applied since functions are "black boxes".







IDEA Termination of Evaluation Strategies ----------------------------------------


important property: if there exists any evaluation sequence that terminates
for a given expression, then call-by-name evaluation will also terminate
for this expression, and produce the same ﬁnal result

note Call by value may not terminate while call by name does.
note But sometimes call by name takes longer.

example

square :: Int → Int
square n = n ∗ n

call-by-value evaluation:
square (1 + 2)
= square 3
= 3 ∗ 3
= 9

call-by-name evaluation:
square (1 + 2)
= (1 + 2) ∗ (1 + 2)
= 3 ∗ (1 + 2)
= 3 * 3
= 9

important property: arguments may be evaluated more than once using call by
name by only once using call by value evaluation.


note: this problem can be solved by keeping one copy of the
argument and make many pointers to it, so when one is solved, all of them are.
 Method is called sharing.

note: sharing + call by name = lazy evaluation. Expressions are evaluated
only as much as required.
-}




-- Example generating infinite list of prime numbers

-- 1. write down infinite sequence 2,3,4,5,6, ...
-- 2. mark first number p as prime
-- 3. delete all multiples of p in the sequence.
-- 4. return to step 2.


-- using sieve of erastothenes
primes :: [Int]
primes = sieve [2..]

sieve        :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]






{-
KEY STRICT APPLICATION

NOTE strict application ($!) means evaluating the top-level argument before
function is applied. Makes it call by value.

example
 square $! (1 + 2)
 ={applying +}
 square $! 3
 ={applying $! }
 square 3
 ={applying
 square }
 3 ∗ 3
 ={applying ∗}
 9

NOTe can be used in any combination when used with curried functions

(f $! x) y --- forces evaluation of x
(f x) $! y --- forces evaluation of y
(f $! x) $! y --- forces evaluation of x and y


example
sumwith :: Int → [ Int ] → Int
sumwith v [] = v
sumwith v (x : xs) = sumwith (v + x ) xs

sumwith 0[1, 2, 3]
={applying
sumwith }
sumwith (0 + 1) [ 2, 3]
={applying sumwith }
sumwith ((0 + 1) + 2) [ 3]
={applying
sumwith }
sumwith (((0 + 1) + 2) + 3) [ ]
={applying
sumwith }
((0 + 1) + 2) + 3
={applying the ﬁrst +}
(1 + 2) + 3
={applying the ﬁrst +}
3 + 3
={applying +}
6

note forces evaluation of accumulator value (more efficient, less memory waste)
sumwith v [] = v
sumwith v (x : xs) = (sumwith $! (v + x )) xs

sumwith 0[1, 2, 3]
={applying
sumwith }
sumwith $! (0 + 1) [ 2, 3]
={applying +}
sumwith $! 1 [ 2, 3]
={applying $! }
sumwith 1[2, 3]
={applying
sumwith }
sumwith $! (1 + 2) [ 3]
={applying +}
sumwith $! 3 [ 3]
={applying $! }
sumwith 3[3]
={applying sumwith }
sumwith $! (3 + 3) [ ]
={applying +}
sumwith $! 6 [ ]
={applying $! }
sumwith 6[]
={applying
sumwith }
6


So no sumwith 0 [1..10000] gives correct result


example
Deﬁning a strict version of the higher-order library function foldl that
forces evaluation of its accumulator prior to processing the tail of the list:

foldl'            :: (a -> b -> a) -> a -> [b] -> a
foldl' f v []     = v
foldl' f v (x:xs) = ((foldl' f) $! (f v x)) xs
-}
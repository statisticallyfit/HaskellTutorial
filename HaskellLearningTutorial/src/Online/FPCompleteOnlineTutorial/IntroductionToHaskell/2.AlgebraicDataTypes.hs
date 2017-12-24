
-- Enumerations
data Thing = Shoe --- these are constructors, values of Thing data type
            | Ship
            | SealingWax
            | Cabbage
            | King
            -- deriving show means automatic generation of code to
            -- convert Thing to String
            deriving Show

-- decarling a shoe
aShoe :: Thing
aShoe = Shoe

-- creating a list
listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]


-- pattern matching with Things
isSmall :: Thing -> Bool
isSmall Ship       = False
isSmall King       = False
isSmall _          = True



-- Beyond enumerations
data FailableDouble = Failure
                     | OK Double -- this means OK with type FailableDouble takes Double argument
                     deriving Show
a = Failure
b = OK 3.4

-- how to use the new FailableDouble type
safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)
-- this is what happens when you bracket the double- double part:
----- The equation(s) for `safeDiv' have two arguments,
----- but its type `(Double -> Double) -> FailableDouble' has only one
----- ==> the first two arguments are x, y and the last is the ouput


failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d







--- Data constructors can have more than one argument
data Person = Person String Int Thing
    deriving Show

brent :: Person
brent = Person "Brent" 30 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _ ) = a



{-}data AlgDataType = Constr1 Type11 Type12
                 | Constr2 Type21
                 | Constr3 Type31 Type32 Type33
                 | Constr4
This specifies that a value of type AlgDataType can be
constructed in one of four ways: using Constr1, Constr2, Constr3,
or Constr4. Depending on the constructor used, an AlgDataType value
may contain some other values. For example, if it was constructed
using Constr1, then it comes along with two values, one of type
Type11and one of type Type12.
-}





--- Pattern Matching
-- using the x@pattern
getName :: Person -> String
getName p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n



-- nesting patterns
checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!" -- because you like wax
checkFav (Person n _ _ )         = n ++ ", your favorite thing is lame."




{-In general, the following grammar defines what can be used as a pattern:

pat ::= _
      | var
      | var @ ( pat )
      | ( Constructor pat1 pat2 ... patn )
The first line says that an underscore is a pattern. The second line says
that a variable by itself is a pattern; such a pattern matches anything,
and “binds” the given variable name to the matched value. The third line
specifies @ patterns. The last line says that a constructor name followed
by a sequence of patterns is itself a pattern; such a pattern matches a
value if that value was constructed using the given constructor, and pat1
through patn all match the values contained by the constructor, recursively.
-}



{- todo: understand better!
Note that literal values like 2 or 'c' can be thought of as constructors
with no arguments. It is as if the types Int and Char were defined like

data Int = 0 | 1 | -1 | 2 | -2 | ...
data Char = 'a' | 'b' | 'c' | ...
which means that we can pattern-match against literal values.
(Of course, Int and Char are not actually defined this way.)
-}





-- Case Expressions
--When evaluated, the expression exp is matched against each of the patterns pat1,
-- pat2, ... in turn. The first matching pattern is chosen, and the entire case
-- expression evaluates to the expression corresponding to the matching pattern.
-- For example,

n = case "Hello" of
  []      -> 3
  ('H':s) -> length s
  _       -> 7
--evaluates to 4 (the second pattern is chosen; the third pattern matches too,
--of course, but it is never reached).




failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
    Failure -> 0
    OK d    -> d





--- Using recursive functions to process Recursive Data Types
-- /show
data IntList = Empty | Cons Int IntList

-- show
intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x xs) = x * intListProd xs





data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))




main = do
    print tree
    print (intListProd (Cons 3 (Cons 2 (Cons 4 Empty))))
    --print (failureToZero' Failure, failureToZero' (OK 3.4))
    --putStrLn (checkFav (Person "Brent" 30 SealingWax))
    --putStrLn (getName brent) -- use putStrLn since value we display is String
    --print(getName brent)
    --print(getAge brent)
    --print (failureToZero Failure, failureToZero (OK 3.4))
    --print (safeDiv 2 0, safeDiv 3 4)
    --print(a, b)
    --print(aShoe)
    --print(listO'Things)
    --print (isSmall (Cabbage))
import Prelude


-- 5.2. TUPLE TYPES -------------------------------------------------------------

-- note types are synonyms to the types they name.


type ShopItem = (String, Int)
--("Salt: 1kg", 139) :: ShopItem

type Basket = [ShopItem]
--[("Salt: 1kg", 139), ("Plain crisps", 25), ("Plain crisps", 25)]


-- use tuple to return compound result
minAndMax :: Integer -> Integer -> (Integer, Integer)
minAndMax x y
    | x >= y    = (y, x)
    | otherwise = (x, y)



fib   :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fibStep        :: (Integer, Integer) -> (Integer, Integer)
fibStep (u, v) = (v, u+v)

fibPair        :: Integer -> (Integer, Integer)
fibPair n
    | n == 0    = (0, 1)
    | otherwise = fibStep (fibPair (n-1))

fastFib   :: Integer -> Integer
fastFib n = fst (fibPair n)
-- help:: todo: the below doesn't work - why?
--fastFib n = fst . (fibPair n)









-- 5.3 ALGEBRAIC DATA TYPES -----------------------------------------------------

-- NOTE advantage over types: they will appear in errors messages since are not
-- synonyms and therefore will not be evaluated into the synonym of the type.


-- (1) enumerated types
data Move = Rock | Paper | Scissors -- note: this is a nullary constructor
            deriving (Eq, Show)


score :: Move -> Move -> Integer
score Rock Rock         = 0
score Paper Paper       = 0
score Scissors Scissors = 0
score Rock Paper        = -1
score Paper Rock        = 1
score Rock Scissors     = 1
score Scissors Rock     = -1
score Paper Scissors    = -1
score Scissors Paper    = 1

-- (2) product types
data People = Person Name Age -- note: this is a binary constructor
              deriving (Eq, Show)
type Name = String
type Age = Int

showPerson :: People -> String
showPerson (Person str n) = str ++ " -- " ++ show n


-- (3) tuples and data types
--type People = (Name, Age)



-- note Person constructor has type:
-- Person :: Name -> Age -> People so can be applied like function.






data Shape = Circle Float | Rectangle Float Float deriving (Eq, Ord, Show)

{-
NOTE: Circle and Rectangle are called constructor functions for type Shape
because elements are constructed by applying these functions.
-}

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

area :: Shape -> Float
area (Circle r) = pi*r*r
area (Rectangle l w) = l*w


{-
NOTE contrast type vs data:

type: synonym for a type, can be expanded out and so removed from the program.
Cannot be recursive.
data: creates a new type, can be recursive. Polymorphic if parametrized by
a type on both left and right (a).
-}

{-
uncover
main = do
    print $ fibStep (0, 1); print $ fibStep (2, 3)----- tuple tupes
    print $ fibStep (9, 14)
    print $ fibPair 5; print $ fibPair 8
    ---------------------------------------------------- algebraic types
    print $ score Rock Scissors
    print $ Person "Electric Aunt Jemima" 77
    print $ Person "Ronnie" 14
    print $ showPerson (Person "Electric Aunt Jemima" 77)
    putStrLn ""
    print $ isRound (Circle 1); print $ isRound (Rectangle 1 2)
    print $ area (Circle 2); print $ area (Rectangle 78 2)
-}













-- 5.7 LIBRARY DATABASE -------------------------------------------------------

type Person = String
type Book   = String

type Database = [(Person, Book)] -- models a list of loans


exampleBase :: Database
exampleBase =
    [ ("Alice", "Tintin"), ("Anna", "Little Women"),
       ("Alice", "Asterix"), ("Rory", "Tintin") ]


{-
NOTE operations
1. given a person, find books borrowed
2. given book, find person (assume more than one copy of book)
3. given book, find whether it IS borrowed.
4.given person, find number of books he or she has borrowed.
-}

-- NOTE: lookup functions
books                    :: Database -> Person -> [Book]
books dBase searchPerson = [b | (p, b) <- dBase, searchPerson == p]

borrowers   :: Database -> Book -> [Person]
borrowers dBase searchBook = [p | (p, b) <- dBase, searchBook == b]

isBorrowed  :: Database -> Book -> Bool
isBorrowed dBase searchBook = length (borrowers dBase searchBook) /= 0

numBorrowed :: Database -> Person -> Int
numBorrowed dBase searchPerson = length (books dBase searchPerson)



-- NOTE: update functions
makeLoan   :: Database -> Person -> Book -> Database
makeLoan dBase p b = [(p,b)] ++ dBase

-- note run through all pairs and retain those not equal to ours.
-- removes all pairs.
returnLoan :: Database -> Person -> Book -> Database
returnLoan dBase p b = [pair | pair <- dBase, pair /= (p,b)]




-- Note testing

-- key if we loan book to person then lookip books loaned to person then book should
-- be in that list.
prop_db1 :: Database -> Person -> Book -> Bool
prop_db1 dBase p b =
    elem b loanedAfterLoan == True
        where
            afterLoan = makeLoan dBase p b
            loanedAfterLoan = books afterLoan p

-- key if we return loan of book to person and then lookip the books loaned to person,
-- then book should not be in that list.
prop_db2 :: Database -> Person -> Book -> Bool
prop_db2 dBase p b =
    elem b loanedAfterReturn == False
        where
            afterReturn = returnLoan dBase p b
            loanedAfterReturn = books afterReturn p
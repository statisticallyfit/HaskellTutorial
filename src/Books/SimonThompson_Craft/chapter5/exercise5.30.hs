type Person = String
type Book = String

data Loan = Loan Person Book deriving (Eq, Show)

type Database = [Loan] -- models a list of loans


exampleBase :: Database
exampleBase =
    [ Loan "Alice" "Tintin", Loan "Anna" "Little Women",
      Loan "Alice" "Asterix", Loan "Rory" "Tintin" ]


{-
NOTE operations
1. given a person, find books borrowed
2. given book, find person (assume more than one copy of book)
3. given book, find whether it IS borrowed.
4.given person, find number of books he or she has borrowed.
-}

-- NOTE: lookup functions
books                    :: Database -> Person -> [Book]
books dBase searchPerson = [b | (Loan p b) <- dBase, searchPerson == p]

borrowers   :: Database -> Book -> [Person]
borrowers dBase searchBook = [p | (Loan p b) <- dBase, searchBook == b]

isBorrowed  :: Database -> Book -> Bool
isBorrowed dBase searchBook = length (borrowers dBase searchBook) /= 0

numBorrowed :: Database -> Person -> Int
numBorrowed dBase searchPerson = length (books dBase searchPerson)



-- NOTE: update functions
makeLoan   :: Database -> Person -> Book -> Database
makeLoan dBase p b = [Loan p b] ++ dBase

-- note run through all pairs and retain those not equal to ours.
-- removes all pairs.
returnLoan :: Database -> Person -> Book -> Database
returnLoan dBase p b = [pair | pair <- dBase, pair /= (Loan p b)]

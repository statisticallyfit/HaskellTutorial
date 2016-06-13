
type Person = String
type Book   = String

type Database = [(Person, Book)] -- models a list of loans


exampleBase :: Database -- not library database, it is database of loans.
exampleBase =
    [ ("Alice", "Tintin"), ("Anna", "Little Women"),
       ("Alice", "Asterix"), ("Rory", "Tintin") ]




books :: Database -> Person -> [Book]
books dBase searchPerson = map snd $ filter isPerson dBase
    where isPerson (p,_) = p == searchPerson

borrowers :: Database -> Book -> [Person]
borrowers dBase searchBook = map fst $ filter isBook dBase
    where isBook (_,b) = b == searchBook

isBorrowed :: Database -> Book -> Bool
isBorrowed dBase searchBook = searchBook `elem` snd (unzip dBase)

isBorrowed' :: Database -> Book -> Bool
isBorrowed' dBase searchBook = any isBook dBase
    where isBook (_,b) = b == searchBook

numBorrowed :: Database -> Person -> Int
numBorrowed dBase searchPerson = length (books dBase searchPerson)
-- length $ filter (== searchPerson) (fst $ unzip dBase)


-- NOTE: update functions
makeLoan :: Database -> Person -> Book -> Database
makeLoan dBase p b = [(p,b)] ++ dBase

-- note run through all pairs and retain those not equal to ours.
-- removes all pairs.
returnLoan :: Database -> Person -> Book -> Database
returnLoan dBase p b = filter (/= (p,b)) dBase


data StudentGrade = Grade Int deriving (Eq, Show) -- percent

data Database = Data [StudentGrade] deriving (Eq, Show)



g1, g2, g3, g4 :: StudentGrade
g1 = Grade 89
g2 = Grade 99
g3 = Grade 94
g4 = Grade 90


d :: Database
d = Data [g1, g2, g3, g4]

main = do
    print d
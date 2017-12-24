

data Deque a = Deque [a] deriving (Eq, Show)


addFront :: a -> Deque a -> Deque a
addFront x (Deque xs) = Deque (x:xs)

addEnd :: a -> Deque a -> Deque a
addEnd x (Deque xs) = Deque (xs ++ [x])

removeFront :: Deque a -> Deque a
removeFront (Deque []) = Deque []
removeFront (Deque (_:xs)) = Deque xs

removeEnd :: Deque a -> Deque a
removeEnd (Deque []) = Deque []
removeEnd (Deque xs) = Deque (init xs)


q1 :: Deque Integer
q1 = Deque [1,2,3,4,5,6,7,8,9,10]


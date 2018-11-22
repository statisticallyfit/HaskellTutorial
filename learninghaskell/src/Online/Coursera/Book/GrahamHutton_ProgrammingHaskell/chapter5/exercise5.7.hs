

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x,y) <- zip xs ys]
-- note: todo: incorrect: scalarProduct xs ys = sum [x * y | x <- xs, y <- ys ]
-- note: todo: correct: g2 = [ (x, y) | x <- [1,2,3], y <- [4,5] ]
-- note: so this means to take the product you need to organize them in a tuple,
-- note: so you cannot do g2 approach then product them. help: are there levels?


main = do
    print $ scalarProduct [1,2,3] [4,5,6]
    print $ scalarProduct [1,2,3,4] [4,5,6]


generator :: [(Integer, Integer)]
generator =  concat [[ (x,y) | y <- [4,5,6]] | x <- [1,2,3]]

main = print generator
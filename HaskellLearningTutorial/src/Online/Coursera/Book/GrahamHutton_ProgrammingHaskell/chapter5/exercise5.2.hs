

replicate'         :: Int -> a -> [a]
replicate' n thing = [thing | _ <- [1 .. n]]


main = do
    print (replicate' 3 True)
    print (replicate' 4 1)
    print (replicate' 5 "abc")
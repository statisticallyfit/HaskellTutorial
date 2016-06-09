
ioSum :: IO Integer
ioSum = do x <- getLine
           y <- getLine
           let firstInt = (read x :: Integer)
           let secondInt = (read y :: Integer)
           return (firstInt + secondInt)

main = do
    ioSum
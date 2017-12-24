
sumSquaresFirstHundred :: Integer
sumSquaresFirstHundred = sum [x^2 | x <- [1 .. 100]]



main = print sumSquaresFirstHundred
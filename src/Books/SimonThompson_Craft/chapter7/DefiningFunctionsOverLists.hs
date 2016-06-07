import Prelude hiding (sum, concat, zip, take)
import qualified Prelude
import Test.QuickCheck

sum        :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs


propSum xs = sum xs == Prelude.sum xs

{-
uncover
main = do
    quickCheck propSum-}




-- 7.4 FINDING PRIMITIVE RECURSION FUNCTIONS --------------------------------------------

concat          :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss

propConcat xss = concat xss == (Prelude.concat xss)




iSort        :: [Integer] -> [Integer]
iSort []     = []
iSort (x:xs) = insert x (iSort xs)

-- precondition: the list y:ys is sorted ascendingly
insert :: Integer -> [Integer] -> [Integer]
insert x []     = [x]
insert x (y:ys)
    | y >= x    = x : y : ys
    | otherwise = y : insert x ys


main = do
    print $ concat [[1,2,3], [4,5]]
    print $ concat ["hi there, ", "how are you?"]
    -- quickCheck propConcat -- help why doesn't print?








-- 7.5 GENERAL RECURSIONS OVER LISTS ----------------------------------------------------

-- NOTE examples of recursion patterns

-- 1
zip :: [a] -> [b] -> [(a,b)]
zip xs []         = []
zip [] ys         = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys


-- 2
take :: Int -> [a] -> [a]
take _ []     = []
take 0 _      = []
take n (x:xs) = x : take (n-1) xs
--take _ _      = error "No negative arg!" -- help where to put this?


 -- 3
qSort :: [Integer] -> [Integer]
qSort [] = []
qSort (x:xs) = qSort [y | y <- xs, y <= x] ++ [x] ++ qSort [y | y <- xs, y > x]
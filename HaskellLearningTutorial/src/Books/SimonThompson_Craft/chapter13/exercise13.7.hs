import Prelude hiding (compare)

class Info a where
    examples :: [a]
    size     :: a -> Int
    size _   = 1


instance Info Int where
    examples = [-100 .. 100]

-- note key - defining examples and size over [a] type
instance Info a => Info [a] where
    -- note has type [[a]]
    examples = [ [] ] ++
               [ [x] | x <- examples ] ++
               [ [x,y] | x <- examples, y <- examples ]
    -- note has type [a]; we need context of Info a => above because we are
    -- using the size that takes an (a) in map size over [a]s .
    size     = foldr (+) 1 . map size





compare :: (Info a, Info b) => a -> b -> Bool
compare x y = size x <= size y




main = do
    print $ compare (1::Int) (10::Int)
    print $ compare ([1,2] :: [Int]) ([3,4,5] :: [Int])
    print $ compare ([1,2,5,6,7,8] :: [Int]) ([3,4,5] :: [Int])
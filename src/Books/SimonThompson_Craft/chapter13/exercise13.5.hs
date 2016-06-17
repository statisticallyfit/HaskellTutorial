{-# OPTIONS_GHC -XFlexibleInstances #-}


class Info a where
    examples :: [a]
    size     :: a -> Int


instance Info (Int -> Bool) where
    examples = [\i -> i == 1]
    size _   = 1

instance Info (Int -> Int) where
    examples = [\i -> i :: Int]
    size _   = 1


{-
HELP
if
:t ex@mples
ex@mples :: Info a => [a]

then why if there is no instance for Info Int, why does this still compile?

-}


main = do
    print $ size (\i -> i == (1 :: Int))
    print $ size ((examples :: [Int -> Int]) !! 0)
    print $ (((examples :: [Int -> Bool]) !! 0) 1)
    print $  (((examples :: [Int -> Bool]) !! 0) 2)
    print $ (((examples :: [Int -> Bool]) !! 1) 2) -- note exception, since length
    -- of the list == 1
    print $ (((examples :: [Int -> Int]) !! 0) 100234)
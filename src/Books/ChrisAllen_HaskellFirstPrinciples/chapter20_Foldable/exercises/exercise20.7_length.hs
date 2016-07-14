


import Data.Foldable
import Data.Monoid


{-
note evaluation

where f = (\acc _ -> acc + 1)

lengthFoldl [1, 2, 3, 4, 5]
= foldl (\acc _ -> acc + 1) 0 [1,2,3,4,5]
= (\0 _ -> 0 + 1) : foldl f 1 [2,3,4,5] -- note seed == 1 comes from 0 + 1 result
= (\1 _ -> 1 + 1) : foldl f 2 [3,4,5] -- note seed == 2 comes from 1+1 result
= (\2 _ -> 2 + 1) : foldl f 3 [4,5]
= (\3 _ -> 3 + 1) : foldl f 4 [5]
= (\4 _ -> 4 + 1) : foldl f 5 [] = 5 since when at foldl _ seed = seed
-}


lengthFoldr :: Foldable t => t a -> Int
lengthFoldr xs = foldr (\_ acc -> acc + 1) 0 xs


lengthFoldl :: Foldable t => t a -> Int
lengthFoldl xs = foldl (\acc _ -> acc + 1) 0 xs


{- -- help help help todo
lengthFoldMap :: Foldable t => t a -> Int
lengthFoldMap xs = foldMap -}


main :: IO()
main = do
    print $ lengthFoldr (1, 2)
    print $ lengthFoldr [(1, 2), (3, 4), (5, 6)]
    print $ fmap lengthFoldr [(1, 2), (3, 4), (5, 6)]
    print $ fmap lengthFoldr (Just [1, 2, 3])
    print $ fmap lengthFoldr [Just 1, Just 2, Just 3]
    print $ fmap lengthFoldr [Just 1, Just 2, Nothing]
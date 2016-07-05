

--- 1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (b:bs) = b || myOr bs

myOrFold :: [Bool] -> Bool
myOrFold = foldr (||) False {-bs-}




--- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
    | f x = True
    | otherwise = myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' _ [] = False
myAny' f (x:xs) = f x || myAny' f xs

myAnyFold :: (a -> Bool) -> [a] -> Bool
myAnyFold f xs = foldr (||) False (map f xs)

myAnyFold' f xs = myOrFold (map f xs)

-- accumulator has already been ff'd, so we just have to f the x then compare.
myAnyFoldr f = foldr (\x acc -> f x || acc) False
myAnyFoldl f = foldl (\acc y -> acc || f y) False





--- 3
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (e:es)
    | x == e = True
    | otherwise = myElem x es

myElem' x es = myAny (== x) es

myElemFoldr :: Eq a => a -> [a] -> Bool
myElemFoldr x es = foldr (\n acc -> n == x || acc) False es

myElemFoldl :: Eq a => a -> [a] -> Bool
myElemFoldl x es = foldl (\acc n -> n == x || acc) False es





--- 4
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myReverse' :: [a] -> [a]
myReverse' = foldl (\acc y -> y : acc) []

parrot :: [a] -> [a]
parrot = foldr (\x acc -> flip (:) acc x) []

--myReverse'' :: [a] -> [a]
--myReverse'' = foldr (\x acc -> acc : x) []
-- note doesn't work because acc is already a list and (:) takes its args the other way




--- 5
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = [f x] ++ myMap f xs

myMapFoldr :: (a -> b) -> [a] -> [b]
myMapFoldr f xs = foldr (\x acc -> [f x] ++ acc) [] xs

myMapFoldl :: (a -> b) -> [a] -> [b]
myMapFoldl f xs = foldl (\acc y -> acc ++ [f y]) [] xs

myMapFoldrNice :: (a -> b) -> [a] -> [b]
myMapFoldrNice f = foldr ((:) . f) []




--- 6
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
    | p x = x : myFilter p xs
    | otherwise = myFilter p xs

myFilterFoldr :: (a -> Bool) -> [a] -> [a]
myFilterFoldr p = foldr f []
    where f x acc
            | p x = x : acc
            | otherwise = acc



--- 7
squishLonger :: [[a]] -> [a]
squishLonger xs = foldr (\x acc -> x ++ acc) [] xs

squish :: [[a]] -> [a]
squish = foldr (++) []




--- 8

lifter x = [x + 1]

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap fLifter xs = squish $ myMap fLifter xs

squishMapFoldr :: (a -> [b]) -> [a] -> [b]
squishMapFoldr fLifter xs = foldr (\x acc -> (fLifter x) ++ acc) [] xs

squishMapFoldl :: (a -> [b]) -> [a] -> [b]
squishMapFoldl fLifter xs = foldl (\acc y -> acc ++ (fLifter y)) [] xs

squishMapNice :: (a -> [b]) -> [a] -> [b]
squishMapNice fLifter = foldr ((++) . fLifter) []
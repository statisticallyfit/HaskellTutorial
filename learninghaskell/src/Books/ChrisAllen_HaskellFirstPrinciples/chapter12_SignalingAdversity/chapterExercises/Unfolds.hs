import Data.List
import Data.Maybe


ex = take 10 $ unfoldr (\b -> Just (b, b+1)) 0


mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
    where go :: Num a => a -> [a] -> a
          go n [] = n
          go n (x:xs) = go (n+x) xs

niceSum :: Num a => [a] -> a
niceSum xs = foldl (+) 0 xs

------------
mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
    where go :: Num a => a -> [a] -> a
          go n [] = n
          go n (x:xs) = go (n*x) xs

niceProduct :: Num a => [a] -> a
niceProduct = foldl (*) 1 -- xs here

---------------
mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
    where go :: [a] -> [[a]] -> [a]
          go xs' [] = xs'
          go xs' (x:xs) = go (xs' ++ x) xs

niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []



------------------------------------------------

--- 1
myIterate :: (a -> a) -> a -> [a]
myIterate f s = s : myIterate f (f s)


--- 2

myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f b = case f b of
                    Nothing -> []
                    Just (x,y) -> x : myUnfoldr f y

-- help todo why is this not the correct type?
myUnfoldr' f next = next : myUnfoldr' f b
            where (_, b) = fromJust $ f next


--- 3

betterIterate :: (a -> a) -> a -> [a]
betterIterate f s = myUnfoldr (\b -> Just (b, f b)) s




------------------------------------------------

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Show)

--- 1
unfoldTree :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfoldTree f s = case f s of
                    Nothing -> Leaf
                    Just (a,b,a') ->  Node (unfoldTree f a) b (unfoldTree f a')

highToLow a = if a == 0 then Nothing else Just (a-1, a, a-1)

--- 2
--- had help here note todo mull over

highTree :: Integer -> BinaryTree Integer
highTree 0 = Leaf
highTree n = unfoldTree f n
    where f m
            | m == 0 = Nothing
            | otherwise = Just (m-1, m, m-1)

lowTree :: Integer -> BinaryTree Integer
lowTree n = unfoldTree f 0
    where f m
            | m == n = Nothing
            | otherwise = Just (m+1, m, m+1)

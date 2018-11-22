-- returns the list of all values associated with the key given to find from
-- the given table
find :: Eq a => a -> [(a, b)] -> [b] -- type 'a' must be instance of Eq class
find key table = [value | (key', value) <- table,  key == key']
----------- return value | such that         it matches my given key

positions      :: Eq a => a -> [a] -> [Int] -- a must implement Eq, be comparable
positions key xs = find key (zip xs [0..n])
                   where n = length xs - 1


main = do
    print $ find 'k' [('a', 1), ('b', 2), ('k', 5), ('i', 4), ('k', 9)]
    print $ positions 8 [1,2,3,4,5,6,7,8,8,8,9,10,10,-1,8,8]
-- Intermission lifting exercises
-- Add fmap, parantheses, and function composition as neeed to make the
-- expression typecheck and produce the expected result.


-- 1
a = fmap (+1) $ read "[1]" :: [Int]

-- 2
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])


-- 3
c = fmap (*2) (\x -> x - 2)


-- 4
d = fmap (( return '1' ++ ) . show) (\x -> [x, 1..3])
d' = ((return '1' ++) . show) . (\x -> [x, 1..3])
d'' = fmap (("1" ++) . show) (\x -> [x, 1..3])
-- keynote: the x list is type [Char] so Char from ('1' ++) won't match with
-- it so that's why you need ("1" ++) instead.
 -- Need return with Char not [Char] argument since:  :t (return '1' ++) is
--             (return '1' ++) :: [Char] -> [Char]
-- so this matches to the [Char] type of show of the lambda



-- 5 -- HELP how to know this was the answer? How to match up the types?

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123" ++ ) . show) ioi -- note fmap needed to get past IO structure from IO Integer
    in fmap (*3) changed -- note: fmap needed maybe to get past Read structure

main = do
    print a
    print b
    print (c 1)
    print (d 0); print (d' 0); print (d'' 0)
    eshow <- e
    print eshow  -- note one way
    fmap show e  -- note a second way to print e HELP


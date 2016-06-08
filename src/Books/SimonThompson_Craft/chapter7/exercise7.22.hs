import Test.QuickCheck


zip' :: ([a], [b]) -> [(a,b)]
zip' (xs, ys) = zip xs ys


--unzip :: [(a,b)] -> ([a], [b])



propZipUnzip (xs,ys) = fst (unzip (zip' (xs,ys))) == xs
                       ||
                       snd (unzip (zip' (xs,ys))) == ys

-- how to test? help
{-propUnzipZip (xs,ys) = fst (zip' (unzip [(xs,ys)])) == xs
                       ||
                       snd (zip' (unzip [(xs,ys)])) == ys-}

--main = quickCheck propZipUnzip -- help doesn't work do with GHCI



composeTwo :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
composeTwo f g = (\x y -> g (f x) (f y))

composeTwo' f g x = g (f x) . f



total :: (Integer -> Integer) -> (Integer -> Integer)
total f = \n -> sum $ map (\x -> f x) [0..n]

total' f = sum . map f . (\n -> [0..n])

total'' f = sum . map f . count
    where count n = [0..n]

total''' f = sum . map f . enumFromTo 0 {-n-} -- enumFromTo 0 5 = [0..5]

pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples limit = [(x,y,z) |  x <- [1 .. limit],
                                       y <- [1 .. limit],
                                       z <- [1 .. limit],
                                       x^2 + y^2 == z^2 ]


main = do
    print $ pythagoreanTriples 10
    print $ pythagoreanTriples 4
    print $ pythagoreanTriples 30

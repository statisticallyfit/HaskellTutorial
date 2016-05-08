-- Find where a straight line crosses the axis

-- y = mx + b,
-- m = slope, b = constant
xIntercept :: Float -> Float -> Float
xIntercept slope constant
    | not (slope == 0) = (-constant) / slope
    | otherwise  = error "The equation does not intersect the x-axis."



main = do
    print $ xIntercept (1/2) (3)
    print $ xIntercept 4 0
    print $ xIntercept 0 10
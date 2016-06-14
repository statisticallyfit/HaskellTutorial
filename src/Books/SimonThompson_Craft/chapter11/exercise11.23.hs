
-- takes function f and calculates its approximation at x.
derivative :: (Double -> Double) -> Double -> Double
derivative = \f x -> ((f (x + delta) - (f x)) / delta)
    where delta = 0.00001


f   :: Double -> Double
f x = x^3 + 2*x^2 + 4*x + 7
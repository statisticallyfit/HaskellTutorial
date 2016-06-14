{-

-- note a <= b
integrate :: (Double -> Double) -> Double -> Double -> Double
integrate a b = \f ->
    where barNum = 100
          barWidth = (b - a) / 100
          -}
-- help  numerical integration formula? todo
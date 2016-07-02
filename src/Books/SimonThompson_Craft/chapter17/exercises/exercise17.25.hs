
--runningSums :: [Int] -> [Int]
runningSums xs = runner xs 0
    where runner [] cum = cum
          runner (x:xs) cum = runner xs (cum + x)
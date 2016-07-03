
-- HELP how to fix this? Not really working...

whileG :: (a -> IO Bool) -> (a -> IO a) -> a -> IO a
whileG testIO toIO x = do test <- testIO x
                          if test
                          then
                            do toIO x
                               whileG testIO toIO x
                          else return x


anIOTest :: Int -> IO Bool
anIOTest x = if x == 20 then return True else return False
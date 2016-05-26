
main = do
    (pure :: a -> IO a) 10
    ((<*>) :: IO (a -> b) -> IO a -> IO b) (return (+1)) (return 3)
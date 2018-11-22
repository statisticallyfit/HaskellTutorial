

getInt :: IO Int
getInt = fmap read getLine


fmap' :: (a -> b) -> IO a -> IO b
fmap' f ioArg = do arg <- ioArg
                   return (f arg)


anIOArg = (return 23) :: IO Int
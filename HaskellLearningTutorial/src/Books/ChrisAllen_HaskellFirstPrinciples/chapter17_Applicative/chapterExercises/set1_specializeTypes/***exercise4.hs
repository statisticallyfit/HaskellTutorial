-- (->) e


t1 = (pure :: a -> ((->) e) a)
t2 = ((<*>) :: ((->) e) (a -> b) -> ((->) e) a -> ((->) e) b)


main = do
    print "" -- HELP how to test this type???
import Control.Monad
import Control.Monad.Trans.State


--newtype State s a = State { runState :: s -> (a, s)}


--- 1
-- help understand meaning of state function
get' :: State s s
get' = state $ \s -> (s, s)


--- 2
put' :: s -> State s () -- value a is unit ()
put' x = state $ \s -> ((), x )


--- 3
exec :: State s a -> s -> s
exec stateSA s = let (a, s') = (runState stateSA) s in s'

--- 4
eval :: State s a -> s -> a
eval stateSA s = let (a, s') = (runState stateSA) s in a

--- 5
modify'' :: (s -> s) -> State s ()
modify'' f = state $ \s -> ((), f s)



main :: IO()
main = do
    print $ runState get' "beachwater"
    print $ runState (put' "lalala") "woot"

    print $ exec (put' "wilma") "daphne"
    print $ exec get' "scooby papu"

    print $ eval get' "bunnicula"
    print $ eval get' "stake a bunny"

    print $ runState (modify'' (+1)) 0
    print $ runState (modify'' (+1) >> modify'' (+1)) 0 -- help todo how does this work?
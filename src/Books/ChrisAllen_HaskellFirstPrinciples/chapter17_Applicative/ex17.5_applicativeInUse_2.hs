import Control.Applicative
import Data.Functor

-- 1
ex1 = const <$> Just "Hello" <*> pure "World"

-- 2
ex2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure ([1,2,3])




main = do
    print ex1
    print ex2
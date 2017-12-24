import Test.QuickCheck
import Test.QuickCheck.Function



functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)




-- 1
data Sum a b = First a | Second b deriving (Eq, Show)


-- note: Sum has kind * -> * -> * so take one arg out in the instance.
instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)


type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool





-- 2
{-
note: it's impossible to apply operation to First a since when we took out
Sum a in the instance type that meant taking out the first arg, which is 'a' in
First a. So we can now only operate on 'b' from Second b, i.e. the second argument.
-}





main = do
    quickCheck $ \x -> functorIdentity (x :: [Int])
    quickCheck (functorIdentity :: [Int] -> Bool )
    quickCheck (functorCompose :: IntFC)
    putStrLn ""
    --print $ fmap (*8) (First 3)
    --print $ fmap (*7) (Second 5)
    -- HELP why error?
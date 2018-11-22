import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



data Bull = Fools | Twoo deriving (Eq, Show)


instance Arbitrary Bull where
    arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

instance EqProp Bull where (=-=) = eq


trigger = undefined :: [(String, String, Int)]

main :: IO ()
main = do
    -- HELP why give just arg Twoo? Why not Bull instead?
    quickBatch (monoid Twoo)
    -- NOTE test whether this tuple type are valid APplicatives
    -- HELP what does    m (a,b,c) -> TestBatch type mean?
    quickBatch $ applicative ( [("b", "w", 1)] :: [(String, String, Int)])
    -- note values passed are not evaluated, just used to know the type to use.
    quickBatch $ applicative trigger
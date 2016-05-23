-- note instead of " a b c"
data Company a c b = DeepBlue a c | Something b deriving (Eq, Show)

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c



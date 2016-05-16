import Control.Applicative ((*>))
import Control.Monad (join)


sequencing :: IO()
sequencing = do
    putStrLn "blah"
    putStrLn "another thing"


-- desugared version 1
sequencing' :: IO()
sequencing' =
    putStrLn "blah" >>
    putStrLn "another thing"

-- desugared version 2
sequencing'' :: IO()
sequencing'' =
    putStrLn "blah" *>
    putStrLn "another thing"


-------------------------------------------------------------------------------------
binding :: IO()
binding = do
    putStr "Input: "
    name <- getLine
    putStrLn name

binding' :: IO()
binding' =
    getLine >>= putStrLn
    -- note: this means instead of naming the variable, we use >>=
    -- to pass it directly.

-------------------------------------------------------------------------------------

-- note: Monad is important for its join.
-- note: join merges the two IO effects
-- notes: goes from String -> IO -> IO(IO())
-- to: String -> IO() -- note: it merges the two IOs
q :: IO (IO ()) -- note: outer IO is for getLine, inner IO is for putStrLn
q = putStrLn <$> getLine
-- now
q' :: IO()
q' = join $ putStrLn <$> getLine

-------------------------------------------------------------------------------------
-- note: desugar do syntax

bindingAndSequencing :: IO()
bindingAndSequencing = do
    putStrLn "name pls: "
    name <- getLine
    putStrLn ("y helo thar: " ++ name)


bindingAndSequencing' :: IO()
bindingAndSequencing' =
    putStrLn "name pls: " >>
    getLine >>= -- note: bind operator should be read as do f1 then f2 and arg is passed indirectly.
    \name -> putStrLn ("y helo thar: " ++ name)




twoBinds :: IO()
twoBinds = do
    putStrLn "name pls: "
    name <- getLine
    putStrLn "age pls: "
    age <- getLine
    putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")


twoBinds' :: IO()
twoBinds' =
    putStrLn "name pls: " >>
    getLine >>=
    \name ->                  -- note: need to do \name and \age separately since we
    putStrLn "age pls: " >>   -- must mark down the user input else it would be
    getLine >>=               -- covered by the next input.
    \age ->
    putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")



twoBinds'' :: IO()
twoBinds'' =
    putStrLn "name pls: " >>
    getLine >>=
        (\name ->                  -- note: need to do \name and \age separately since we
        putStrLn "age pls: " >>   -- must mark down the user input else it would be
        getLine >>=               -- covered by the next input.
        (\age ->
        putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")))



------------------------------------------------------------------------------------
-- 18.4 Examples of Monad use

-- LIST -----------------------------------------------------------------------------
{-
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>=) ::            [a] -> (a -> [b]) -> [b]

return :: Monad m => a -> m a
return ::            a -> [a]
-}
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x then [x^2, x^2] else [x^2]

-- note: the do hides the join of the List monad
twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
    x <- xs
    if even x then [x^2, x^2] else []


d xs = [[x^2,x^2] | x <- xs, even x]






-- MAYBE ---------------------------------------------------------------------------
{-
note: m ~ Maybe
(>>=) :: Monad m => m a -> (a ->     m b) ->     m b
(>>=) ::        Maybe a -> (a -> Maybe b) -> Maybe b

return :: Monad m => a ->     m a
return ::            a -> Maybe a
-}


data Cow = Cow {
    name   :: String,
    age    :: Int,
    weight :: Int
} deriving (Eq, Show)

noEmpty     :: String -> Maybe String
noEmpy ""   = Nothing
noEmpty str = Just str

noNegative               :: Int -> Maybe Int
noNegative n | n >= 0    = Just n
             | otherwise = Nothing

weightCheck   :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
        in if n == "Bess" && w > 499
        then Nothing
        else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
    case noEmpty name' of
    Nothing    -> Nothing
    Just nammy ->
        case noNegative age' of
            Nothing   -> Nothing
            Just agey ->
                case noNegative weight' of
                    Nothing      -> Nothing
                    Just weighty ->
                        weightCheck (Cow nammy agey weighty)


-- note: cleaning up function with monad
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
    nammy   <- noEmpty name'
    agey    <- noNegative age'
    weighty <- noNegative weight'
    weightCheck (Cow nammy agey weighty)


mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' = do
    noEmpty name' >>=
        \nammy ->
        noNegative age' >>=
            \agey ->
            noNegative weight' >>=
                \weighty ->
                weightCheck (Cow nammy agey weighty)


{-
example: passing some arguments:

mkSphericalCow'' "Bess" 5 499
    noEmpty "Bess" >>=
        \nammy ->
        noNegative 5 >>=
            \agey ->
            noNegative 499 >>=
                \weighty ->
                weightCheck (Cow nammy agey weight)



mkSphericalCow'' "Bess" 5 499
    noEmpty "Bess" >>=
        \"Bess" ->              equals: Just "Bess" but with >>= it is "Bess"
        noNegative 5 >>=
            \agey ->
            noNegative 499 >>=
                \weighty ->
                weightCheck (Cow nammy agey weight)



mkSphericalCow'' "Bess" 5 499
    noEmpty "Bess" >>=
        \"Bess" ->
        noNegative 5 >>=
            \5 ->               equals: Just 5 but with >>= it is (5)
            noNegative 499 >>=
                \weighty ->
                weightCheck (Cow nammy agey weight)


mkSphericalCow'' "Bess" 5 499
    noEmpty "Bess" >>=
        \"Bess" ->
        noNegative 5 >>=
            \5 ->
            noNegative 499 >>=
                \499 ->          equals: Just 499 but with >>= it is (499)
                weightCheck (Cow nammy agey weight)


-- note: but if had a "" for name?
mkSphericalCow'' "" 5 499
    noEmpty "" >>=    equals: Nothing >>= _ so the entire computation drops.
        \nammy ->
        noNegative 5 >>=
            \agey ->
            noNegative 499 >>=
                \weighty ->
                weightCheck (Cow nammy agey weight)

summary: entire computation drops any moment that any function in the Maybe Monad
actions produce Nothing, because of:

instance Monad Maybe where
    return x = Just x
    (Just x) >>= k     = k x    key so run the entire computation over x
    Nothing >>=  _     NOthing  key so the entire computation is dropped
-}

-- note: if you return() something at the end of this type of pattern,
-- then use Applicative but if you just have a statement then that is going to
-- produce more monadic structure so you need the implicit monad join to crunch
-- it back down.
-- EXAMPLE

f :: Maybe Integer
f = Just 1
g :: Maybe String
g = Just "1"
h :: Maybe Integer
h = Just 10191

zed :: a -> b -> c -> (a, b, c)
zed = (,,)

doSomething = do
    a <- f
    b <- g
    c <- h
    return (zed a b c)

zedM :: Monad m => a -> b -> c -> m (a, b, c)
zedM a b c = return (a, b, c)

doSomethingM = do
    a <- f
    b <- g
    c <- h
    zedM a b c

-- help: how does this relate to above text? how does it get crunched?





-- EITHER --------------------------------------------------------------------------

{-
note: m ~ Either e
(>>=) :: Monad m => m a -> (a ->        m b) ->        m b
(>>=) ::     Either e a -> (a -> Either e b) -> Either e b

return :: Monad m => a ->        m a
return ::            a -> Either e a

-}
-- years ago
type Founded = Int
-- number of programmers
type Coders = Int

data SoftwareShop =
    Shop {
        founded     :: Founded,
        programmers :: Coders
    } deriving (Eq, Show)

data FoundedError = NegativeYears Founded
                  | TooManyYears Founded
                  | NegativeCoders Coders
                  | TooManyCoders Coders
                  | TooManyCodersForYears Founded Coders
                  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
    | n < 0     = Left $ NegativeYears n
    | n > 500   = Left $ TooManyYears n
    | otherwise = Right n


validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0     = Left $ NegativeCoders n
    | n > 5000  = Left $ TooManyCoders n
    | otherwise = Right n


mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
    founded     <- validateFounded years
    programmers <- validateCoders coders
    if programmers > founded `div` 10
        then Left $ TooManyCodersForYears founded programmers
        else Right $ Shop founded programmers







-- note: todo: finish this chapter + exercises later.


main = do
    print $ d [1..10]
    print $ join $ d [1..10]
    (print . join . d) [1..10]
    putStrLn ""
    --------------------------------------
    print $ mkSphericalCow "Bess" 5 499
    print $ mkSphericalCow "Bess" 5 500

    print $ mkSphericalCow' "Bess" 5 499
    print $ mkSphericalCow' "Bess" 5 500
    putStrLn ""
    --------------------------------------
    print $ mkSoftware 0 0
    print $ mkSoftware (-1) 0
    print $ mkSoftware (-1) (-1)
    print $ mkSoftware 0 (-1)
    print $ mkSoftware 500 0
    print $ mkSoftware 501 0
    print $ mkSoftware 501 501
    print $ mkSoftware 100 5001
    print $ mkSoftware 0 500

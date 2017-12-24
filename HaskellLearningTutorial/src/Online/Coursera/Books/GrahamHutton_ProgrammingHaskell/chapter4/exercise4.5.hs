{-}Do the same for the following version, and note the difference
in the number of conditional expressions required:
True ∧ b  = b
False ∧ _ = False
-}

op :: Bool -> Bool -> Bool
op a b = if a then b else False


main = print (op True True, op True False, op False True, op False False)
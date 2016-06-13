

-- INDUCTION PROPOSITION:
-- foldr1 g ( xs ++ ys ) = g ( foldr1 g xs ) ( foldr1 g ys )


-- foldr g s ( xs ++ ys ) = g ( foldr g s xs ) ( foldr g s ys )
-- (1. Assumption)  <=>  s `g` x = x
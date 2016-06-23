

{-
NOTE we put the last case with [] last not first because we want the Leaf case to match first
so that we don't skip last letter.

'b' : 'a' : 't' : 't' : 'a' : decode (Leaf 't' 0) []
= 'b' : 'a' : 't' : 't' : 'a' : 't'
= "battat"
-}
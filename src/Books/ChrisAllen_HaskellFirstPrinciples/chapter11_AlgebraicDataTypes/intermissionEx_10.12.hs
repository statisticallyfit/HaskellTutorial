

--- 1
data Quad = One | Two | Three | Four deriving (Eq, Show)

-- Sum types are additive so: can take on 2 * 4 forms = 8
eQuad :: Either Quad Quad
eQuad = Left One
eQuad = Left Two
eQuad = Left Three
eQuad = Left Four
eQuad = Right One
eQuad = Right Two
eQuad = Right Three
eQuad = Right Four



--- 2

-- Product types are multiplicative so: can take on 4 * 4 = 16 forms
propQuad :: (Quad, Quad)
propQuad = undefined


--- 3

-- Function types are exponential so: can take on 4 ^ 4 = 256
funcQuad :: Quad -> Quad
funcQuad = undefined


--- 4

-- Product type: 2 * 2 * 2 = 8
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined 
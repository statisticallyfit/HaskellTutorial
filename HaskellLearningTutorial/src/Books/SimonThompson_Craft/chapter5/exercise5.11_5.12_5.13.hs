data NewShape = NewCircle Float (Float, Float)
                  | NewRectangle Float Float (Float, Float)
                  | NewTriangle Float Float Float (Float, Float)
                  deriving(Eq, Ord, Show)


move :: Float -> Float -> NewShape -> NewShape
move x y (NewCircle r (a, b)) = NewCircle r (x+a, y+b)
move x y (NewRectangle l w (a, b)) = NewRectangle l w (x+a, y+b)
move x y (NewTriangle s1 s2 s3 (a, b)) = NewTriangle s1 s2 s3 (x+a, y+b)


{-

note Two circles intersect if, and only if, the distance between their centers
is between the sum and the difference of their radii.

note

note
-}

d x1 y1 x2 y2 = sqrt( (x2 - x1)^2 + (y2 - y1)^2)

{- HELP not good fix later. Succeeds cases
1. completely inside
2. partial overlap
Fails
3. no overlap or intersect.
isOverlap :: NewShape -> NewShape -> Bool
isOverlap (NewCircle r1 (x1, y1)) (NewCircle r2 (x2, y2))
        =  dist <= (r2 + r1)
        where dist = sqrt( (x2 - x1)^2 + (y2 - y1)^2)-}

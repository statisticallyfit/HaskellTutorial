

-- exercise 22 / 23 b

-- positioned representation (two positions which give bottom left and top right
-- corners of a area in pixel).

type BottomRight = (Int, Int)
type TopLeft = (Int, Int)
type Area = (TopLeft, BottomRight)
type Pixel  = Char
type Bitmap = Position -> Pixel

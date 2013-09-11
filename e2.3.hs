data Point = Point {x :: Float, y :: Float} deriving Show
data Rectangle = RectangleFromPoints Point Point | RectangleFromPointAndDimensions Point Float Float deriving Show

width (RectangleFromPoints lowerLeft upperRight) = (x upperRight - x lowerLeft)
width (RectangleFromPointAndDimensions lowerLeft w h) = w

height (RectangleFromPoints lowerLeft upperRight) = (y upperRight - y lowerLeft)
height (RectangleFromPointAndDimensions lowerLeft w h) = h

area r = width r * height r
perimeter r = 2 * (width r + height r)

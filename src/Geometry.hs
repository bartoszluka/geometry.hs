module Geometry (
    lineThrough,
    mkLine,
    intersection,
    doubleEq,
    Line (..),
    Point,
    baroCenter,
    perpendicularThrough,
    toStr,
    isParallelTo,
) where

import Text.Printf (printf)

type Point = (Double, Double)

data Line
    = -- | x = c
      Vertical Double
    | -- | y = ax + b
      Line
        ( Double -- a
        , Double -- b
        )
mkLine :: Double -> Double -> Line
mkLine = curry Line

-- mkLine a b = Line (a, b)

epsilon :: Double
epsilon = 1e-5

toStr :: Line -> String
toStr (Vertical x) =
    printf "x = %.2f" x
-- y = 2.00x + 5.00
-- y = -2.00x - 0.69
toStr (Line (a, b)) =
    printf "y = %.2fx%s%.2f" a (if b < 0 then " - " else " + ") (abs b)

instance Show Line where
    show = toStr

instance Eq Line where
    (Vertical x1) == (Vertical x2) = doubleEq x1 x2
    (Line (a1, b1)) == (Line (a2, b2)) = doubleEq a1 a2 && doubleEq b1 b2
    _ == _ = False

doubleEq :: Double -> Double -> Bool
doubleEq x y = abs (x - y) < epsilon

lineThrough :: Point -> Point -> Line
lineThrough (xa, ya) (xb, yb) =
    case xa - xb of
        0 -> Vertical xa
        _nonZero ->
            Line
                ( (ya - yb) / (xa - xb)
                , (ya - yb) / (xa - xb) * xb - yb
                )

baroCenter :: Point -> Point -> Point -> Point
baroCenter pointA pointB pointC =
    intersection height1 height2
  where
    height1 = perpendicularThrough pointC (lineThrough pointA pointB)
    height2 = perpendicularThrough pointB (lineThrough pointA pointC)

intersection :: Line -> Line -> Point
intersection (Line (a1, b1)) (Line (a2, b2)) =
    let a = -(b1 - b2) / (a1 - a2)
     in (a, a * a1 + b1)
intersection (Line (a, b)) (Vertical x) =
    (x, a * x + b)
intersection (Vertical x) (Line (a, b)) =
    (x, a * x + b)
intersection (Vertical _x) (Vertical _x') = let infinity = 1 / 0 in (infinity, infinity)

perpendicularThrough :: Point -> Line -> Line
perpendicularThrough (_px, py) (Vertical _) = Line (0, py)
perpendicularThrough (px, py) (Line (a, _)) =
    if a == 0
        then Vertical px
        else Line (-1 / a, py + px / a)

isParallelTo :: Line -> Line -> Bool
isParallelTo (Line (a1, _)) (Line (a2, _)) =
    doubleEq a1 a2
isParallelTo (Vertical _) (Vertical _) = True
isParallelTo _ _ = False


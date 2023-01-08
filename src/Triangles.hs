{-# LANGUAGE NamedFieldPuns #-}

module Triangles (
    lineThrough,
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

data Line = Vertical Double | Line {coefA :: Double, coefB :: Double}

epsilon :: Double
epsilon = 1e-5

toStr :: Line -> String
toStr (Vertical x) =
    printf "x = %.2f" x
-- y = 2.00x + 5.00
-- y = -2.00x - 0.69
toStr (Line{coefA, coefB}) =
    printf "y = %.2fx%s%.2f" coefA (if coefB < 0 then " - " else " + ") (abs coefB)

instance Show Line where
    show = toStr

instance Eq Line where
    (Vertical x1) == (Vertical x2) = doubleEq x1 x2
    (Line{coefA = a1, coefB = b1}) == (Line{coefA = a2, coefB = b2}) = doubleEq a1 a2 && doubleEq b1 b2
    _ == _ = False

doubleEq :: Double -> Double -> Bool
doubleEq x y = abs (x - y) < epsilon

lineThrough :: Point -> Point -> Line
lineThrough (xa, ya) (xb, yb) =
    case xa - xb of
        0 -> Vertical xa
        _nonZero ->
            Line
                { coefA = (ya - yb) / (xa - xb)
                , coefB = (ya - yb) / (xa - xb) * xb - yb
                }

baroCenter :: Point -> Point -> Point -> Point
baroCenter pointA pointB pointC =
    intersection height1 height2
  where
    height1 = perpendicularThrough pointC (lineThrough pointA pointB)
    height2 = perpendicularThrough pointB (lineThrough pointA pointC)

intersection :: Line -> Line -> Point
intersection (Line{coefA = a1, coefB = b1}) (Line{coefA = a2, coefB = b2}) =
    let a = -(b1 - b2) / (a1 - a2)
     in (a, a * a1 + b1)
intersection (Line{coefA = a, coefB = b}) (Vertical x) =
    (x, a * x + b)
intersection (Vertical x) (Line{coefA = a, coefB = b}) =
    (x, a * x + b)
intersection (Vertical _x) (Vertical _x') = let infinity = 1 / 0 in (infinity, infinity)

perpendicularThrough :: Point -> Line -> Line
perpendicularThrough (_px, py) (Vertical _) = Line{coefA = 0, coefB = py}
perpendicularThrough (px, py) (Line{coefA = a, coefB = _}) =
    if a == 0
        then Vertical px
        else Line{coefA = -1 / a, coefB = py + 1 / a * px}

isParallelTo :: Line -> Line -> Bool
isParallelTo (Line{coefA = a1}) (Line{coefA = a2}) =
    doubleEq a1 a2
isParallelTo (Vertical _) (Vertical _) = True
isParallelTo _ _ = False
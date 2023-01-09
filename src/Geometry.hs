{-# LANGUAGE NamedFieldPuns #-}
module Geometry (
    onTheSameLine,
    lineThrough,
    pointEq,
    Circle (..),
    CircleCreationError (..),
    mkLine,
    intersection,
    doubleEq,
    Line (..),
    Point,
    baroCenter,
    perpendicularThrough,
    toStr,
    isParallelTo,
    circleThroughPoints,
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

data Circle = Circle {center :: Point, radius :: Double}

instance Eq Circle where
    (Circle{center = c1, radius = r1}) == (Circle{center = c2, radius = r2}) =
        c1 `pointEq` c2 && r1 `doubleEq` r2

instance Show Circle where
    show (Circle{center = (x, y), radius}) =
        printf "center: (%.2f, %.2f), radius: %.2f" x y radius

data CircleCreationError
    = PointsOnTheSameLine (Point, Point, Point)
    | TwoPointsAreTheSame (Point, Point)
    | ThreePointsAreTheSame (Point, Point, Point)
    deriving (Show, Eq)

circleThroughPoints :: Point -> Point -> Point -> Either CircleCreationError Circle
circleThroughPoints p1@(x1, y1) p2@(x2, y2) p3@(x3, y3)
    | p1 `pointEq` p2 && p2 `pointEq` p3 = Left $ ThreePointsAreTheSame (p1, p2, p3)
    | p1 `pointEq` p2 = Left $ TwoPointsAreTheSame (p1, p2)
    | p2 `pointEq` p3 = Left $ TwoPointsAreTheSame (p2, p3)
    | p1 `pointEq` p3 = Left $ TwoPointsAreTheSame (p1, p3)
    | onTheSameLine p1 p2 p3 = Left $ PointsOnTheSameLine (p1, p2, p3)
    | otherwise =
        Right $
            -- calculations
            -- (x - x1^2) + (y - y2^2) = r^2
            -- (x - x2^2) + (y - y2^2) = r^2
            -- (x - x3^2) + (y - y3^2) = r^2
            --
            -- (x - x1^2) + (y - y2^2) = (x - x2^2) + (y - y2^2)
            -- (x - x1^2) + (y - y2^2) - (x - x2^2) - (y - y2^2) = 0
            -- x^2 - 2*x*x1 + x1^2 + y^2 - 2*y*y1 + y1^2 - x^2 - 2*x*x2 - x2^2 - y^2 - 2*y*y2 - y2^2 = 0
            -- - 2*x*x1 + x1^2 - 2*y*y1 + y1^2 - 2*x*x2 - x2^2 - 2*y*y2 - y2^2 = 0
            -- 2*x*x1 = x1^2 - 2*y*y1 + y1^2 - 2*x*x2 - x2^2 - 2*y*y2 - y2^2
            -- 2*x*x1 + 2*x*x2 = x1^2 - 2*y*y1 + y1^2 - x2^2 - 2*y*y2 - y2^2
            -- x*2*(x1+x2) = x1^2 - 2*y*y1 + y1^2 - x2^2 - 2*y*y2 - y2^2
            -- x = (x1^2 - 2*y*y1 + y1^2 - x2^2 - 2*y*y2 - y2^2) / 2*(x1+x2)
            --
            -- (x - x3^2) + (y - y3^2) = r^2
            -- - 2*x*x3 + x3^2 - 2*y*y3 + y3^2 - 2*x*x2 - x2^2 - 2*y*y2 - y2^2 = 0
            -- - 2*x*(x2+x3) + x3^2 - 2*y*y3 + y3^2 - x2^2 - 2*y*y2 - y2^2 = 0
            -- -((x1^2 - 2*y*y1 + y1^2 - x2^2 - 2*y*y2 - y2^2) / (x1+x2))*(x2+x3) + x3^2 - 2*y*y3 + y3^2 - x2^2 - 2*y*y2 - y2^2 = 0
            -- - x1^2/ (x1+x2)*(x2+x3) + 2*y*y1 / (x1+x2)*(x2+x3) - y1^2/ (x1+x2)*(x2+x3) + x2^2/ (x1+x2)*(x2+x3) + 2*y*y2/ (x1+x2)*(x2+x3) + y2^2/ (x1+x2)*(x2+x3)  + x3^2 - 2*y*y3 + y3^2 - x2^2 - 2*y*y2 - y2^2 = 0
            -- 2*y*y1 / (x1+x2)*(x2+x3)+ 2*y*y2/ (x1+x2)*(x2+x3) - 2*y*y3 - 2*y*y2 - x1^2/ (x1+x2)*(x2+x3)  - y1^2/ (x1+x2)*(x2+x3) + x2^2/ (x1+x2)*(x2+x3)  + y2^2/ (x1+x2)*(x2+x3)  + x3^2 + y3^2 - x2^2 - y2^2 = 0
            -- y*(2*y1 / (x1+x2)*(x2+x3)+ 2*y2/ (x1+x2)*(x2+x3) - 2*y3 - 2*y2) - x1^2/ (x1+x2)*(x2+x3)  - y1^2/ (x1+x2)*(x2+x3) + x2^2/ (x1+x2)*(x2+x3)  + y2^2/ (x1+x2)*(x2+x3)  + x3^2 + y3^2 - x2^2 - y2^2 = 0
            -- - y*(2*y1 / (x1+x2)*(x2+x3)+ 2*y2/ (x1+x2)*(x2+x3) - 2*y3 - 2*y2) = - x1^2/ (x1+x2)*(x2+x3)  - y1^2/ (x1+x2)*(x2+x3) + x2^2/ (x1+x2)*(x2+x3)  + y2^2/ (x1+x2)*(x2+x3)  + x3^2 + y3^2 - x2^2 - y2^2
            -- - y = (- x1^2/ (x1+x2)*(x2+x3)  - y1^2/ (x1+x2)*(x2+x3) + x2^2/ (x1+x2)*(x2+x3)  + y2^2/ (x1+x2)*(x2+x3)  + x3^2 + y3^2 - x2^2 - y2^2) / (2*y1 / (x1+x2)*(x2+x3)+ 2*y2/ (x1+x2)*(x2+x3) - 2*y3 - 2*y2)

            let y =
                    -( -x1 ** 2
                        / (x1 + x2)
                        * (x2 + x3)
                        - y1 ** 2 / (x1 + x2) * (x2 + x3)
                        + x2 ** 2 / (x1 + x2) * (x2 + x3)
                        + y2 ** 2 / (x1 + x2) * (x2 + x3)
                        + x3 ** 2
                        + y3 ** 2
                        - x2 ** 2
                        - y2 ** 2
                     )
                        / ( 2 * y1 / (x1 + x2) * (x2 + x3)
                                + 2 * y2 / (x1 + x2) * (x2 + x3)
                                - 2 * y3
                                - 2 * y2
                          )
                x = (x1 ** 2 - 2 * y * y1 + y1 ** 2 - x2 ** 2 - 2 * y * y2 - y2 ** 2) / 2 * (x1 + x2)
                r = sqrt ((x - x1) ** 2 + (y - y1) ** 2)
             in Circle{center = (x, y), radius = r}

pointEq :: (Double, Double) -> (Double, Double) -> Bool
pointEq (x1, y1) (x2, y2) = x1 `doubleEq` x2 && y1 `doubleEq` y2

onTheSameLine :: Point -> Point -> Point -> Bool
onTheSameLine p1 p2 p3 = lineThrough p1 p2 == lineThrough p2 p3

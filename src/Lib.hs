module Lib (
    lineThrough,
    intersection,
    Line (..),
    Point,
    baroCenter,
    toStr,
) where

import Text.Printf (printf)

type Point = (Double, Double)

newtype Line = Line (Double, Double, Double) deriving (Show)

toStr :: Line -> String
toStr (Line (0, 0, _)) =
    "impossible"
toStr (Line (0, b, c)) =
    printf "y = %.2f" (-c / b)
toStr (Line (a, 0, c)) =
    printf "x = %.2f" (-c / a)
toStr (Line (a, b, c)) =
    printf "y = %.2fx + %.2f" (-a / b) (-c / b)

lineThrough :: Point -> Point -> Line
lineThrough (xa, ya) (xb, yb) =
    case (ya - yb) of
        0 -> Line (0, 1, -ya)
        nonZero ->
            Line
                ( xa - xb
                , ya - yb
                , ya * (ya - yb) - xa * (xa - xb)
                )

baroCenter :: Point -> Point -> Point -> Point
baroCenter pointA pointB pointC =
    intersection height1 height2
  where
    height1 = perpendicularThrough pointC (lineThrough pointA pointB)
    height2 = perpendicularThrough pointB (lineThrough pointA pointC)

intersection :: Line -> Line -> Point
intersection (Line (a1, b1, c1)) (Line (a2, b2, c2)) =
    ( (c2 * b1 - c1 * b2) / (a1 * b2 - a2 * b1)
    , (c2 * a1 - c1 * a2) / (b1 * a2 - b2 * a1)
    )

perpendicularThrough :: Point -> Line -> Line
-- perpendicularThrough (px, py) (Line (a, b, c)) = Line (-1 / newa, 1, (-py + 1 / newa * px)) -- works
perpendicularThrough (px, py) (Line (a, b, c)) = Line (-b, a, b * px - a * py)

main :: IO ()
main = do
    putStrLn $ toStr $ lineThrough (0, 0) (1, 1)
    putStrLn $ show $ baroCenter (0, 0) (0, 3) (4, 0)
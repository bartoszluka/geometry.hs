{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Test.Hspec
import Test.QuickCheck

import Geometry (
    Circle (..),
    CircleCreationError (..),
    Line (..),
    Point (..),
    baroCenter,
    circleThroughPoints,
    insideCircle,
    intersection,
    isOnTheCircle,
    isParallelTo,
    lineThrough,
    mkLine,
    onTheSameLine,
    perpendicularThrough,
    tangentThrough,
    (!~),
 )

instance Arbitrary Line where
    arbitrary = do
        n <- chooseInt (1, 10)
        if n == 1
            then Vertical <$> arbitrary
            else do
                a <- arbitrary
                b <- arbitrary
                return $ Line (a, b)

instance Arbitrary Point where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Point (a, b)

main :: IO ()
main = hspec $ do
    describe "lineThrough" $ do
        it "should give the same line if the same points are given in opposite order" $
            property $
                \(p1, p2) -> p1 /= p2 ==> lineThrough p1 p2 `shouldBe` lineThrough p2 p1

    describe "intersection" $ do
        it "should give the same point if the same lines are given in opposite order" $
            property $ \(line1, line2) ->
                (line1 /= line2 && not (line1 `isParallelTo` line2))
                    ==> (intersection line1 line2 `shouldBe` intersection line2 line1)

    describe "perpendicularThrough" $ do
        it "gives the line that's perpendicular to a given line and goes through a given point" $ do
            (mkLine 0.5 0 `perpendicularThrough` Point (2, 1)) `shouldBe` mkLine (-2) 5
            (Vertical 5 `perpendicularThrough` Point (2, 1)) `shouldBe` mkLine 0 1

        it "gives back line parallel to the provided one if applied twice" $ do
            property $ \(line, point) ->
                let perpendicular = flip perpendicularThrough point
                 in (perpendicular . perpendicular) line `shouldSatisfy` isParallelTo line

        it "gives back the same line if applied twice and through the intersection with the first perpendicular line" $ do
            property $ \(startingLine, point) ->
                let secondLine = startingLine `perpendicularThrough` point
                    intersectionPoint = intersection secondLine startingLine
                 in (secondLine `perpendicularThrough` intersectionPoint) `shouldBe` startingLine

    describe "baroCenter" $ do
        it "gives barometric center of a triangle" $ do
            baroCenter (Point (0, 0)) (Point (3, 0)) (Point (0, 4)) `shouldBe` Point (0, 0)
            baroCenter (Point (0, 0)) (Point (4, 0)) (Point (2, 4)) `shouldBe` Point (2.0, 1.0)
            -- equilateral
            baroCenter (Point (0, 0)) (Point (6, 0)) (Point (3, 6 * sqrt 3 / 2)) `shouldBe` Point (3, sqrt 3)

    describe "circleThroughPoints" $ do
        it "returns a circle through points (-2, 0) (0, 2) (2, 0)" $
            circleThroughPoints (Point (-2, 0)) (Point (0, 2)) (Point (2, 0))
                `shouldBe` (Right $ Circle{center = Point (0, 0), radius = 2})

        it "returns a circle that does not have any NaN values" $ do
            property $ \(x, y1, y2, y3) ->
                (y1 `notElem` [y2, y3] && x !~ 0)
                    ==> let
                            noNaNs (p1, p2, p3) = case circleThroughPoints p1 p2 p3 of
                                Right (Circle{center = Point{coordinates = (x0, y)}, radius}) ->
                                    (not . any isNaN) [x0, y, radius]
                                Left _ -> False
                         in
                            do
                                (Point (x, y1), Point (-x, y2), Point (-x, y3)) `shouldSatisfy` noNaNs
                                (Point (-x, y1), Point (x, y2), Point (-x, y3)) `shouldSatisfy` noNaNs
                                (Point (-x, y1), Point (-x, y2), Point (x, y3)) `shouldSatisfy` noNaNs
                                (Point (-x, y1), Point (x, y2), Point (x, y3)) `shouldSatisfy` noNaNs
                                (Point (x, y1), Point (-x, y2), Point (x, y3)) `shouldSatisfy` noNaNs
                                (Point (x, y1), Point (x, y2), Point (-x, y3)) `shouldSatisfy` noNaNs

        it "returns a valid circle through 3 distinct points around 0" $
            property $ \p ->
                let
                 in (p !~ 0)
                        ==> circleThroughPoints (Point (-p, 0)) (Point (0, p)) (Point (p, 0))
                        `shouldBe` (Right $ Circle{center = Point (0, 0), radius = abs p})

        it "gives an error if 2 points are the same" $
            property $ \(p1, p2) ->
                (p1 /= p2)
                    ==> (circleThroughPoints p1 p1 p2 `shouldBe` Left (TwoPointsAreTheSame (p1, p1)))

        it "gives an error if all 3 points are the same" $
            property $ \(p1 :: Point) ->
                circleThroughPoints p1 p1 p1 `shouldBe` Left (ThreePointsAreTheSame (p1, p1, p1))

        it "gives a circle with positive radius when 3 correct points are provided" $
            property $ \(p1, p2, p3) ->
                (p1 /= p2 && p2 /= p3 && p1 /= p3 && not (onTheSameLine p1 p2 p3))
                    ==> case circleThroughPoints p1 p2 p3 of
                        Right Circle{radius} ->
                            radius `shouldSatisfy` (> 0)
                        Left err -> expectationFailure $ "preconditions are not exhaustive; edge case found with error: " ++ show err

    describe "tangent" $ do
        it "returns a line that's perpendicular to a radius going through an intersection point" $
            property $ \(center, radius, point) ->
                let circle = Circle center radius
                    intersectionWithRadiusIsOnTheCircles (Right (line1, line2)) =
                        (intersection line1 (line1 `perpendicularThrough` center) `isOnTheCircle` circle)
                            && (intersection line2 (line2 `perpendicularThrough` center) `isOnTheCircle` circle)
                    intersectionWithRadiusIsOnTheCircles (Left _) = False
                 in not (point `insideCircle` circle)
                        ==> (circle `tangentThrough` point)
                        `shouldSatisfy` intersectionWithRadiusIsOnTheCircles

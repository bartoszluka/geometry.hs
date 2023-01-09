{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Monad (liftM2)
import Test.Hspec
import Test.QuickCheck

import Geometry (
    Circle (..),
    CircleCreationError (..),
    Line (..),
    Point (..),
    baroCenter,
    circleThroughPoints,
    intersection,
    isParallelTo,
    lineThrough,
    mkLine,
    onTheSameLine,
    perpendicularThrough,
 )

instance Arbitrary Line where
    arbitrary = do
        let or' = liftM2 (||)
            randDouble :: Gen Double
            randDouble = arbitrary `suchThat` (not . (isInfinite `or'` isNaN))
        n <- chooseInt (1, 10)
        if n == 1
            then Vertical <$> randDouble
            else do
                a <- randDouble
                b <- randDouble
                return $ mkLine a b

instance Arbitrary Point where
    arbitrary = do
        let or' = liftM2 (||)
            randDouble :: Gen Double
            randDouble = arbitrary `suchThat` (not . (isInfinite `or'` isNaN))
        a <- randDouble
        b <- randDouble
        return $ Point (a, b)

main :: IO ()
main = hspec $ do
    describe "lineThrough" $ do
        it "should give the same line if the same points are given in opposite order" $ do
            property $ \((p1, p2) :: (Point, Point)) -> lineThrough p1 p2 `shouldBe` lineThrough p2 p1

    describe "intersection" $ do
        it "should give the same point if the same lines are given in opposite order" $ do
            property $ \((line1, line2) :: (Line, Line)) ->
                (line1 /= line2 && not (line1 `isParallelTo` line2))
                    ==> (intersection line1 line2 `shouldBe` intersection line2 line1)

    describe "perpendicularThrough" $ do
        it "gives the line that's perpendicular to a given line and goes through a given point" $ do
            perpendicularThrough (Point (2, 1)) (mkLine 0.5 0) `shouldBe` mkLine (-2) 5
            perpendicularThrough (Point (2, 1)) (Vertical 5) `shouldBe` mkLine 0 1
        it "gives back the same line if applied twice" $ do
            property $ \((startingLine, point) :: (Line, Point)) ->
                let secondLine = perpendicularThrough point startingLine
                    intersectionPoint = intersection secondLine startingLine
                 in perpendicularThrough intersectionPoint secondLine `shouldBe` startingLine

    describe "baroCenter" $ do
        it "gives barometric center of a triangle" $ do
            baroCenter (Point (0, 0)) (Point (3, 0)) (Point (0, 4)) `shouldBe` Point (0, 0)
            baroCenter (Point (0, 0)) (Point (4, 0)) (Point (2, 4)) `shouldBe` Point (2.0, 1.0)
            -- equilateral
            baroCenter (Point (0, 0)) (Point (6, 0)) (Point (3, 6 * sqrt 3 / 2)) `shouldBe` Point (3, sqrt 3)

    describe "circleThroughPoints" $ do
        it "returns a circle through 3 distinct points" $ do
            circleThroughPoints (Point (-2, 0)) (Point (0, 2)) (Point (2, 0)) `shouldBe` (Right $ Circle{center = Point (0, 0), radius = 2})
        it "gives an error if 2 points are the same" $ do
            property $ \((p1, p2) :: (Point, Point)) ->
                (p1 /= p2)
                    ==> (circleThroughPoints p1 p1 p2 `shouldBe` Left (TwoPointsAreTheSame (p1, p1)))
        it "gives an error if all 3 points are the same" $ do
            property $ \(p1 :: Point) ->
                circleThroughPoints p1 p1 p1 `shouldBe` Left (ThreePointsAreTheSame (p1, p1, p1))
        it "gives a circle with positive radius when 3 correct points are provided" $ do
            property $ \((p1, p2, p3) :: (Point, Point, Point)) ->
                (p1 /= p2 && p2 /= p3 && not (onTheSameLine p1 p2 p3))
                    ==> case circleThroughPoints p1 p2 p3 of
                        Right Circle{radius} ->
                            radius `shouldSatisfy` (>= 0)
                        Left err -> expectationFailure $ "preconditions are not exhaustive; edge case found with error: " ++ show err

infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) g f x y = g (f x y)

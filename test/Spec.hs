{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

import Test.Hspec (describe, hspec, it, shouldBe, shouldSatisfy)
import Test.QuickCheck
import Triangles (Line (..), Point, baroCenter, doubleEq, intersection, lineThrough, perpendicularThrough)

instance Arbitrary Line where
    arbitrary = do
        let randDouble :: Gen Double
            randDouble = suchThat arbitrary (\x -> not (isInfinite x || isNaN x))
        n <- chooseInt (1, 10)
        if n == 1
            then Vertical <$> randDouble
            else do
                a <- randDouble
                b <- randDouble
                return $ Line a b

main :: IO ()
main = hspec $ do
    describe "lineThrough" $ do
        it "should give the same line if the same points are given in opposite order" $ do
            property $ \((p1, p2) :: (Point, Point)) -> lineThrough p1 p2 `shouldBe` lineThrough p2 p1

    describe "intersection" $ do
        it "should give the same point if the same lines are given in opposite order" $ do
            property $ \((line1, line2) :: (Line, Line)) ->
                (line1 /= line2)
                    ==> let (x1, y1) = intersection line1 line2
                            (x2, y2) = intersection line2 line1
                         in do
                                x1 `shouldSatisfy` doubleEq x2
                                y1 `shouldSatisfy` doubleEq y2

    describe "perpendicularThrough" $ do
        it "gives the line that's perpendicular to a given line and goes throug a given point" $ do
            perpendicularThrough (2, 1) (Line 0.5 0) `shouldBe` Line (-2) 5
            perpendicularThrough (2, 1) (Vertical 5) `shouldBe` Line 0 1
        it "applies perpendicularThrough twice to get the same line" $ do
            property $ \((startingLine, point) :: (Line, Point)) ->
                let secondLine = perpendicularThrough point startingLine
                    intersectionPoint = intersection secondLine startingLine
                 in perpendicularThrough intersectionPoint secondLine `shouldBe` startingLine

    describe "baroCenter" $ do
        it "gives barometric center of a triangle" $ do
            baroCenter (0, 0) (3, 0) (0, 4) `shouldBe` (0, 0)
import Lib (Point, intersection)

main :: IO ()
main = hspec $ do
    describe "intersection" $ do
        it "should give the same line if the same points are given in opposite order" $ do
            property $ \(p1, p2 :: (Point, Point)) -> intersection p1 p2 `shouldBe` intersection p2 p1

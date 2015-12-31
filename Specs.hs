import Test.Hspec
import PythTree

margin = 0.00001

shouldBeApprox (x,y) (x',y') = abs (x - x') < margin && abs (y - y') < margin

main = hspec $ do
    describe "opposite angle" $ do
        it "given hypothenuse finds the point of opposite angle" $ do
            oppositeAngle 10.0 (pi/4)  `shouldBeApprox`  (5.0,5.0)

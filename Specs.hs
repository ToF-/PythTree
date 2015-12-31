import Test.Hspec
import PythTree

margin = 0.00001

shouldBeApprox (x,y) (x',y') = (rounded x,rounded y) `shouldBe` (rounded x', rounded y')

rounded x = (fromIntegral (round (x*10000)))/ 10000

main = hspec $ do
    describe "opposite angle" $ do
        it "given hypothenuse finds the point of opposite angle" $ do
            oppositeAngle 10.0 (pi/4)  `shouldBeApprox`  (5.0,5.0)
            oppositeAngle 10.0 (pi/8)  `shouldBeApprox`  (8.5355,3.5355) 


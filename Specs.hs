import Test.Hspec
import PythTree

margin = 0.00001

shouldBeApprox (x,y) (x',y') = (rounded x,rounded y) `shouldBe` (x',y')

shouldBeApproxShape pts pts' = map (\(x,y) -> (rounded x, rounded y)) pts `shouldBe` pts'

rounded x = (fromIntegral (round (x*10000)))/ 10000

main = hspec $ do
    describe "opposite angle" $ do
        it "given hypothenuse finds the point of opposite angle" $ do
            oppositeAngle 1.0 (pi/4)  `shouldBeApprox`  (0.5,0.5)
            oppositeAngle 10.0 (pi/8)  `shouldBeApprox`  (8.5355,3.5355) 
            oppositeAngle 100.0 (pi/3) `shouldBeApprox` (25.0,43.3013)

    describe "square and triangle" $ do 
        it "given a size and angle, yields a square and rectangle triangle" $ do
            squareAndTriangle 10.0 (pi/8)  `shouldBeApproxShape` 
                [(0.0, 0.0)
                ,(10.0, 0.0)
                ,(10.0, 10.0)
                ,(8.5355, 13.5355)
                ,(0.0, 10.0)]



import Test.Hspec
import PythTree

margin = 0.00001

shouldBeApprox (x,y) (x',y') = (rounded x,rounded y) `shouldBe` (x',y')

shouldBeApproxShape (Shape pts) (Shape pts') 
    = map (\(x,y) -> (rounded x, rounded y)) pts `shouldBe` pts'

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
                Shape [(0.0, 0.0)
                      ,(10.0, 0.0)
                      ,(10.0, 10.0)
                      ,(8.5355, 13.5355)
                      ,(0.0, 10.0)]

    describe "toTikz" $ do
        it "converts a shape to TikZ commands" $ do
            toTikz (Shape [(-5,-2),(3,1),(-4,4)]) `shouldBe` 
                "\\path[fill] (-5.0,-2.0) -- (3.0,1.0) -- (-4.0,4.0) -- cycle;\n"

            toTikz (Shape [(5,2),(3,1),(4,4)]) `shouldBe` 
                "\\path[fill] (5.0,2.0) -- (3.0,1.0) -- (4.0,4.0) -- cycle;\n"

    describe "a shape" $ do
        it "can be a composition of shapes" $ do
            let c = Shapes [Shape [(-5,-2),(3,1),(-4,4)]
                           ,Shape [(5,2),(3,1),(4,4)]]
                c' = Shapes [Shape [(0,0),(10,0),(0,10)], c]
            toTikz c `shouldBe` 
                unlines ["\\path[fill] (-5.0,-2.0) -- (3.0,1.0) -- (-4.0,4.0) -- cycle;"
                        ,"\\path[fill] (5.0,2.0) -- (3.0,1.0) -- (4.0,4.0) -- cycle;"]
           
            toTikz c' `shouldBe` 
                unlines ["\\path[fill] (0.0,0.0) -- (10.0,0.0) -- (0.0,10.0) -- cycle;"
                        ,"\\path[fill] (-5.0,-2.0) -- (3.0,1.0) -- (-4.0,4.0) -- cycle;"
                        ,"\\path[fill] (5.0,2.0) -- (3.0,1.0) -- (4.0,4.0) -- cycle;"]

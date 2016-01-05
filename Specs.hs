import Test.Hspec
import PythTree

margin = 0.00001

shouldBeApprox (x,y) (x',y') = (rounded x,rounded y) `shouldBe` (x',y')

shouldBeApproxShape (Shape pts) (Shape pts') 
    = map (\(x,y) -> (rounded x, rounded y)) pts `shouldBe` pts'


main = hspec $ do
    describe "opposite angle" $ do
        it "given hypothenuse finds the point of opposite angle" $ do
            oppositeAngle 1.0 (pi/4)  `shouldBeApprox`  (0.5,0.5)
            oppositeAngle 10.0 (pi/8)  `shouldBeApprox`  (8.53553,3.53553) 
            oppositeAngle 100.0 (pi/3) `shouldBeApprox` (25.0,43.30127)

    describe "square and triangle" $ do 
        it "given a size and angle, yields a square and rectangle triangle" $ do
            squareAndTriangle 10.0 (pi/8)  `shouldBeApproxShape` 
                Shape [(0.0, 0.0)
                      ,(10.0, 0.0)
                      ,(10.0, 10.0)
                      ,(8.53553, 13.53553)
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

        it "can be a translation of a shape" $ do
            let t = Translate (100.0, 150.0) (Shapes [Shape [(0,0),(10,0),(0,10)]
                                                     ,Shape [(-50,-50),(-60,-50),(-50,-40)]])
            toTikz t `shouldBe` 
                unlines ["\\path[fill] (100.0,150.0) -- (110.0,150.0) -- (100.0,160.0) -- cycle;"
                        ,"\\path[fill] (50.0,100.0) -- (40.0,100.0) -- (50.0,110.0) -- cycle;"]

        it "can be a rotation of a shape" $ do
            let r = Rotate (0.0,0.0) (pi/6) (Shapes [Shape [(0,0),(10,0),(0,10)]
                                                    ,Shape [(2,2),(8,2),(2,8)]])
            toTikz r `shouldBe` 
                unlines ["\\path[fill] (0.0,0.0) -- (8.66025,5.0) -- (-5.0,8.66025) -- cycle;"
                        ,"\\path[fill] (0.73205,2.73205) -- (5.9282,5.73205) -- (-2.26795,7.9282) -- cycle;"]

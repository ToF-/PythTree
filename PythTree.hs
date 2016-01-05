module PythTree
where
import Data.List (intersperse)

rounded x = (fromIntegral (round (x*100000)))/ 100000

type Size  = Float
type Angle = Float
type Point = (Float, Float)
data Shape = Shape [Point]
           | Shapes [Shape]
           | Translate Point Shape
           | Rotate Point Angle Shape

oppositeAngle :: Size -> Angle -> Point
oppositeAngle  h a = (h * cos a * cos a, h * cos a * sin a)

squareAndTriangle :: Size -> Angle -> Shape
squareAndTriangle s a = 
    Shape [(0.0, 0.0)
          ,(s  , 0.0)
          ,(s  , s  )
          ,(x ,  s+y)
          ,(0.0, s)]
        where (x,y) = oppositeAngle s a

toTikz :: Shape -> String
toTikz = unlines . toTikz'

toTikz' :: Shape -> [String]
toTikz' (Translate pt (Shapes shs)) = toTikz' (Shapes (map (Translate pt) shs))  
toTikz' (Translate pt (Shape pts)) = toTikz' (Shape (map (translate pt) pts))
    where translate (x,y) (x',y') = (x+x',y+y')
toTikz' (Rotate pt a (Shapes shs)) = toTikz' (Shapes (map (Rotate pt a) shs))  
toTikz' (Rotate pt a (Shape pts)) = toTikz' (Shape (map (rotate pt a) pts))
    where rotate (x0,y0) a (x, y) = (x*cos a - y*sin a + (x0-x0*cos a + y0*sin a)
                                    ,x*sin a + y*cos a + (y0-x0*sin a - y0*cos a))
toTikz' (Shapes shs) = concatMap toTikz' shs
toTikz' (Shape pts) = ["\\path[fill] " ++ showAllPoints pts ++ " -- cycle;"]
    where showAllPoints :: [Point] -> String
          showAllPoints pts = concat (intersperse " -- " (map show (map (\(x,y) -> (rounded x, rounded y)) pts)))



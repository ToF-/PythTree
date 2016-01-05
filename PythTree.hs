module PythTree
where
import Data.List (intersperse)

type Size  = Float
type Angle = Float
type Point = (Float, Float)
data Shape = Shape [Point]
           | Shapes [Shape]

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

drawWithTikz :: Shape -> String
drawWithTikz = unlines . drawWithTikz'

drawWithTikz' :: Shape -> [String]
drawWithTikz' (Shapes shs) = concatMap drawWithTikz' shs
drawWithTikz' (Shape pts) = ["\\path[fill] " ++ showAllPoints pts ++ " -- cycle;"]
    where showAllPoints :: [Point] -> String
          showAllPoints pts = concat (intersperse " -- " (map show pts))



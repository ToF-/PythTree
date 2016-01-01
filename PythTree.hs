module PythTree
where

type Point = (Float, Float)
data Shape = Shape [Point]
oppositeAngle  h a = (h * cos a * cos a, h * cos a * sin a)

squareAndTriangle s a = 
    Shape [(0.0, 0.0)
          ,(s  , 0.0)
          ,(s  , s  )
          ,(x ,  s+y)
          ,(0.0, s)]
        where (x,y) = oppositeAngle s a

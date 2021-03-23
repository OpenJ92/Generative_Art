module Shape (Shape (Rectangle, Ellipse, RtTriangle, Polygon), Radius, Side, Vertex, square, circle, distBetween, area, produceEquilateralTriangle, produceEquilateralTriangle180 ) where

type Vertex = (,) Float Float
type Radius = Float
type Side   = Float

data Shape 
 = Rectangle Side Side
 | Ellipse Radius Radius
 | RtTriangle Side Side
 | Polygon [Vertex]
 deriving (Show)

addVertex :: Vertex -> Vertex -> Vertex
addVertex (x,y) (x',y') = (x+x', y+y')

subtractVertex :: Vertex -> Vertex -> Vertex
subtractVertex (x,y) (x',y') = (x-x', y-y')

square :: Side -> Shape
square s = Rectangle s s

circle :: Radius -> Shape
circle r = Ellipse r r

rectangle :: Side -> Side -> Shape
rectangle a b = Polygon [(0, 0), (a, 0), (0, b), (a, b)]

rtTriangle :: Side -> Side -> Shape
rtTriangle a b = Polygon [(0, 0), (a, 0), (0, b)] 

regularPolygon :: Int -> Side -> Shape
regularPolygon n side = Polygon $ scanl1 addVertex (constructRegularPolygonSides n (pi - measureRegularAngle n) (0, side))

measureRegularAngle :: Int -> Float
measureRegularAngle count = (((count' - 2) * 180) * pi) / (count' * 180)
  where
    count' = fromIntegral count

constructRegularPolygonSides :: Int -> Float -> Vertex -> [Vertex]
constructRegularPolygonSides 0 _ _ = []
constructRegularPolygonSides n theta vertex@(x, y) =
  let vertex' = (cos(theta)*x - sin(theta)*y, sin(theta)*x + cos(theta)*y)
  in vertex : constructRegularPolygonSides (n-1) theta vertex'

area :: Shape -> Float
area (Rectangle s s') = s * s'
area (RtTriangle s s') = s * s' / 2
area (Ellipse r r') = r * r' * pi
area (Polygon (x:y:z:rs)) = triArea x y z + area (Polygon (x:z:rs))
area (Polygon _) = 0

distBetween :: Vertex -> Vertex -> Float
distBetween x y = let (z, z') = subtractVertex x y in sqrt $ z^2 + z'^2

triArea :: Vertex -> Vertex -> Vertex -> Float
triArea x y z =
  let a = distBetween x y
      b = distBetween y z
      c = distBetween z x
      s = (a + b + c)*(0.5)
  in sqrt (s * (s - a) * (s - b) * (s - c))

produceEquilateralTriangle :: Vertex -> Float -> [Vertex]
produceEquilateralTriangle (x, y) size 
  = [
      (x , y - 2*(size/2)), 
      (x + (size), y + (sqrt 3)*(size/2)), 
      (x - (size), y + (sqrt 3)*(size/2))
    ]

produceEquilateralTriangle180 :: Vertex -> Float -> [Vertex]
produceEquilateralTriangle180 (x, y) size 
  = [
      (x , y + 2*(size/2)), 
      (x + (size), y - (sqrt 3)*(size/2)), 
      (x - (size), y - (sqrt 3)*(size/2))
    ]

module Shape where

import Graphics.SOE

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

distance :: Vertex -> Vertex -> Float
distance x y = let (z, z') = subtractVertex x y in sqrt $ z^2 + z'^2

crossProduct :: Vertex -> Vertex -> Float
crossProduct (a,b) (c,d) = a*d - b*c

-- This resolves the included angle of the given triangle.
triArea :: Vertex -> Vertex -> Vertex -> Float
triArea x y z =
  let u = subtractVertex x y
      v = subtractVertex z y
  in asin (crossProduct u v / ((distance x y)*(distance x z)))

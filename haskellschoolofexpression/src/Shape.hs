module Shape (Shape (Rectangle, Ellipse, RtTriangle, Polygon), Radius, Side, Vertex, square, circle, distBetween, area, produceEquilateralTriangle, produceEquilateralTriangle180, eqTriTransform, apply', itterateTransform) where

import Data.List

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

multiplyVertex :: Vertex -> Float -> Vertex
multiplyVertex (x,y) a = (a*x, a*y)
 
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
      (x - (size), y + (sqrt 3)*(size/2)),
      (x , y - 2*(size/2))
    ]

produceEquilateralTriangle180 :: Vertex -> Float -> [Vertex]
produceEquilateralTriangle180 (x, y) size 
  = [
      (x , y + 2*(size/2)), 
      (x + (size), y - (sqrt 3)*(size/2)), 
      (x - (size), y - (sqrt 3)*(size/2)),
      (x , y + 2*(size/2)) 
    ]

apply' :: (Vertex -> Vertex -> [Vertex]) -> [Vertex] -> [Vertex]
apply' f (x:y:[]) = f x y ++ [y]
apply' f (x:y:zs)  = f x y ++ apply' f (y:zs)

rotate :: Float -> Vertex -> Vertex
rotate theta (x,y) = (cos(theta)*x - sin(theta)*y, sin(theta)*x + cos(theta)*y)

eqTriTransform :: Vertex -> Vertex -> [Vertex]
eqTriTransform x y = 
  let y'           = subtractVertex y x
      (a:b:[])     = (multiplyVertex) y' <$> [1/3, 2/3]
      c'           = addVertex a $ rotate (angleToRadian (-60)) a
      ys''         = a:c':b:[]
      ys           = addVertex x <$> ys''
  in x:ys

angleToRadian :: Float -> Float
angleToRadian angle = pi * angle / 180

itterateTransform :: (Vertex -> Vertex -> [Vertex]) -> Int -> [Vertex] -> [Vertex]
itterateTransform f n = foldl (.) (id) $ (take n . repeat) (apply' f)

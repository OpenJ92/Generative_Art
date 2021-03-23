module SimpleGraphics where

import Shape

import Graphics.SOE
import Data.Tree
import Data.List

spaceClose :: Graphics.SOE.Window -> IO ()
spaceClose w 
  = do k <- Graphics.SOE.getKey w
       if k ==' ' then Graphics.SOE.closeWindow w
                  else spaceClose w

redEllipse :: Graphics.SOE.Graphic
redEllipse = Graphics.SOE.withColor Graphics.SOE.Red (Graphics.SOE.ellipse (150, 150) (300, 200))

bluePolyLine :: Graphics.SOE.Graphic
bluePolyLine = Graphics.SOE.withColor Graphics.SOE.Blue (Graphics.SOE.polyline [(100,50), (200,50), (200,250), (100,250), (100,50)])

fillTri :: Graphics.SOE.Window -> Int -> Int -> Int -> IO ()
fillTri w x y size =
  Graphics.SOE.drawInWindow w (Graphics.SOE.withColor Graphics.SOE.Blue (Graphics.SOE.polygon [(x,y), (x + size, y), (x, y - size), (x, y)]))

fillPolygon :: Graphics.SOE.Window -> [(Int, Int)] -> IO ()
fillPolygon w vs = Graphics.SOE.drawInWindow w (Graphics.SOE.withColor Graphics.SOE.White (Graphics.SOE.polygon vs))

minSize :: Int
minSize = 2

sierpinskiTri :: Graphics.SOE.Window -> Int -> Int -> Int -> IO ()
sierpinskiTri w x y size
  = if size <= minSize
       then fillTri w x y size
       else let size2 = size `div` 2
            in do sierpinskiTri w x y size2
                  sierpinskiTri w x (y - size2) size2
                  sierpinskiTri w (x + size2) y size2

cochSnowflakeTree :: Graphics.SOE.Window -> Int -> Int -> Int -> Tree (IO ())
cochSnowflakeTree w x y size = Node (fillPolygon w label) construct
  where
    label@[(a,b), (c,d), (e,f)] = pixelateVertex <$> produceEquilateralTriangle (fromIntegral x, fromIntegral y) (fromIntegral size)
    nsize = div size 3
    construct =
      if nsize >= minSize
         then [ cochSnowflakeTree w a b nsize
              , cochSnowflakeTree w c d nsize
              , cochSnowflakeTree w e f nsize
              ]
         else []

cochSnowflake :: Window -> Int -> Int -> Int -> IO ()
cochSnowflake w x y size = sequence_ $ flatten $ cochSnowflakeTree w x y size

pixelateVertex :: Vertex -> (Int, Int)
pixelateVertex (x, y) = (,) (round x) (round y)

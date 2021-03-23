module Main where

import Lib
import Shape
import SimpleGraphics

import Graphics.SOE

main :: IO ()
main = Graphics.SOE.runGraphics (
  do w <- Graphics.SOE.openWindow "Hello World!" (600, 600)
     cochSnowflake w 300 300 200 
     -- sierpinskiTri w 50 300 256
     -- sequence_ $ Graphics.SOE.drawInWindow w <$> [bluePolyLine, redEllipse]
     -- SimpleGraphics.fillPolygon w $ SimpleGraphics.pixelateVertex <$> Shape.produceEquilateralTriangle (150, 150) 100
     spaceClose w )

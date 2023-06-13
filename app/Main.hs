module Main where

import Vec3D
import Ray
import Util
import qualified Config

-- Take a Color and convert it into a .ppm format compatible string.
colorString :: Color -> String
colorString color = show r ++ " " ++ show g ++ " " ++ show b
    where Vec3D r g b = mapV (fromInteger . floor) $ (255.999 |*| color)

-- For a given point on the screen, calculate its color based on
-- the defined scenario.
generateColor :: Int -> Int -> Color
generateColor x y = rayColor ray
    where u = (x /. (Config.imageWidth - 1)) :: Double
          v = (y /. (Config.imageHeight - 1)) :: Double
          ray = Ray Config.originPoint (
            Config.lowerLeftCorner +
            (u |*| Config.horizontalPoint) +
            (v |*| Config.verticalPoint) -
            Config.originPoint)

-- Create and write to disk an image in the .ppm format according
-- to the scenario defined in `generateColor`.
main :: IO ()
main = do
    let image = imageHeader ++ rawData in
        writeFile "image.ppm" $ unlines image
    where imageHeader = ["P3", formattedSize, "255"]
          formattedSize = show Config.imageWidth ++ " " ++ show Config.imageHeight

          rawData = map colorString [generateColor x y | y <- ys, x <- xs]
          -- ys and xs are all points on the screen.
          ys = [Config.imageHeight - 1, Config.imageHeight - 2 .. 0]
          xs = [0 .. Config.imageWidth - 1]

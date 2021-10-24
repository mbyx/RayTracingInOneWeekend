module Main where

import Vec3
import Ray
import Util
import qualified Config

writeColor :: Color -> IO ()
writeColor color = putStrLn fmtedColor
    where fmtedColor = show r ++ " " ++ show g ++ " " ++ show b
          r = floor $ 255.999 * (getX color)
          g = floor $ 255.999 * (getY color)
          b = floor $ 255.999 * (getZ color)

generateColor :: Int -> Int -> Color
generateColor x y = rayColor ray
    where u = (x /. (Config.imageWidth - 1)) :: Double
          v = (y /. (Config.imageHeight - 1)) :: Double
          ray = Ray Config.originPoint (
            Config.lowerLeftCorner +
            (multScalar Config.horizontalPoint u) +
            (multScalar Config.verticalPoint v) -
            Config.originPoint)

main = do
    putStrLn "P3"
    putStrLn $ show Config.imageWidth ++ " " ++ show Config.imageHeight
    print 255

    mapM_ writeColor [generateColor x y | y <- ys, x <- xs]
    where ys = [Config.imageHeight - 1, Config.imageHeight - 2 .. 1]
          xs = [0 .. Config.imageWidth - 1]

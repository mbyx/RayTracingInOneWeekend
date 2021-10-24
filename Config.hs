module Config where

import Vec3

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

imageWidth :: Int
imageWidth = 400

imageHeight :: Int
imageHeight = floor $ (((fromIntegral imageWidth) :: Double) / aspectRatio)

viewportHeight :: Double
viewportHeight = 2.0

viewportWidth :: Double
viewportWidth = aspectRatio * viewportHeight

focalLength :: Double
focalLength = 1.0

originPoint :: Point
originPoint = Vec3 0.0 0.0 0.0

horizontalPoint :: Point
horizontalPoint = Vec3 viewportWidth 0.0 0.0

verticalPoint :: Point
verticalPoint = Vec3 0.0 viewportHeight 0.0

lowerLeftCorner :: Point
lowerLeftCorner = originPoint -
    (divScalar horizontalPoint 2.0) -
    (divScalar verticalPoint 2.0) -
    (Vec3 0.0 0.0 focalLength)

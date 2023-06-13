{-# LANGUAGE ScopedTypeVariables #-}

module Config where

import Vec3D
import Util

aspectRatio :: Double = 16.0 / 9.0

imageWidth :: Int = 400
imageHeight :: Int = floor $ ((fromIntegral imageWidth) :: Double) / aspectRatio

viewportHeight :: Double = 2.0
viewportWidth :: Double = aspectRatio * viewportHeight

focalLength :: Double = 1.0

originPoint :: Point = Vec3D 0.0 0.0 0.0

horizontalPoint :: Point = Vec3D viewportWidth 0.0 0.0
verticalPoint :: Point = Vec3D 0.0 viewportHeight 0.0

lowerLeftCorner :: Point = originPoint -
    (horizontalPoint |/| 2.0) -
    (verticalPoint |/| 2.0) -
    (Vec3D 0.0 0.0 focalLength)

module Vec3D where

import Data.Text
import Text.Printf

-- Assume that a point on the screen or the color at that point is a vector.
type Color = Vec3D
type Point = Vec3D

-- A vector with an x, y, and z component with double precision.
data Vec3D = Vec3D { x :: Double, y :: Double, z :: Double }

-- Show a vector as a coordinate triple: (x, y, z)
instance Show Vec3D where
  show (Vec3D x y z) = printf "(%d, %d, %d)" x y z :: String

-- Calculate the sum of the magnitudes of the components of the vector.
-- This is alternatively the magnitude of the resultant squared.
lenSqr :: Vec3D -> Double
lenSqr (Vec3D x y z) = (x * x) + (y * y) + (z * z)

-- Calculate the magnitude of the given vector.
len :: Vec3D -> Double
len v = sqrt $ lenSqr v

-- Calculate the scalar product of two vectors.
dotProduct :: Vec3D -> Vec3D -> Double
dotProduct (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) =
    (x1 * x2) + (y1 * y2) + (z1 * z2)

-- Calculate the vector product between two vectors.
crossProduct :: Vec3D -> Vec3D -> Vec3D
crossProduct (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) =
    Vec3D ((y1 * z2) - (z1 * y2)) ((z1 * x2) - (x1 * z2)) ((x1 * y2) - (y1 * x2))

-- Create a new vector that is the unit vector of the given vector.
-- This is a vector that has a magnitude of 1.0.
unit :: Vec3D -> Vec3D
unit v = v |/| len v

-- Multiply a vector by a scalar value.
(|*|) :: Double -> Vec3D -> Vec3D
s |*| (Vec3D x y z) = Vec3D (x * s) (y * s) (z * s)

-- Divide a vector by a scalar value.
(|/|) :: Vec3D -> Double -> Vec3D
v |/| s = (1 / s) |*| v

mapV :: (Double -> Double) -> Vec3D -> Vec3D
mapV f v = Vec3D (f $ x v) (f $ y v) (f $ x v)

-- Allow Vec3D to be treated as a normal mathematical object.
instance Num Vec3D where
    (Vec3D x1 y1 z1) + (Vec3D x2 y2 z2) = Vec3D (x1 + x2) (y1 + y2) (z1 + z2)
    (Vec3D x1 y1 z1) - (Vec3D x2 y2 z2) = Vec3D (x1 - x2) (y1 - y2) (z1 - z2)
    v1 * v2 = crossProduct v1 v2
    abs v = mapV abs v
    signum v = mapV signum v
    fromInteger i = Vec3D (fromInteger i) (fromInteger i) (fromInteger i)

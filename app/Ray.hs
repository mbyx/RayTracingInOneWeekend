module Ray where

import Vec3
import Util

data Ray = Ray Point Vec3

getOrigin :: Ray -> Point
getOrigin (Ray origin _) = origin

getDirection :: Ray -> Vec3
getDirection (Ray _ direction) = direction

at :: Ray -> Double -> Point
(Ray origin direction) `at` t = origin + (multScalar direction t)

rayColor :: Ray -> Color
rayColor ray @ (Ray _ direction) = if hitsSphere (Vec3 0.0 0.0 (-1.0)) 0.5 ray
    then Vec3 1.0 0.0 0.0
    else (
    ((Vec3 1.0 1.0 1.0) `multScalar` (1.0 - t)) +
    ((Vec3 0.5 0.7 1.0) `multScalar` t))
    where unitDirection = unit direction
          t = 0.5 * ((getY unitDirection) + 1.0)

type Radius = Double
hitsSphere :: Point -> Radius -> Ray -> Bool
hitsSphere center radius ray = discriminant > 0
    where discriminant = (b * b) - (4 * a * c)
          a = dotProduct (getDirection ray) (getDirection ray)
          b = 2.0 * (dotProduct oc (getDirection ray))
          c = (dotProduct oc oc) - (radius * radius)
          oc = (getOrigin ray) - center

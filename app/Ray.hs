module Ray where

import Vec3D
import Util

-- A Ray is a beam of light originating at the `source` with some `direction`.
data Ray = Ray { source :: Point, direction :: Vec3D }

-- Determines the point that the ray has travelled to after a timestep `t`.
-- This is similar to the vector equation of a line: r = a + tb
at :: Ray -> Double -> Point
(Ray source direction) `at` t = source + (t |*| direction)

-- This is the scenario generator for the image. For a given ray,
-- it determines what color to project.
rayColor :: Ray -> Color
rayColor ray@(Ray _ direction) = 
    -- Configure the center and radius of the sphere as needed.
    let sphereCenter = Vec3D 0.0 0.0 (-1.0)
        sphereRadius = 0.5

        -- Determine the x coordinate of the collision of ray and sphere.
        collisionPoint = hitsSphere sphereCenter sphereRadius ray

        -- Calculate the unit vector normal at the surface of sphere.
        unitNormal = unit $ ((ray `at` t) - (Vec3D 0.0 0.0 (-1.0)))
        
        -- Determine the timestep and direction of the ray.
        unitDirection = unit direction
        t = 0.5 * ((y unitDirection) + 1.0) in

        case collisionPoint of
            Just p -> Vec3D (x unitNormal + 1) (y unitNormal + 1) (z unitNormal + 1) |/| 2
            Nothing -> (((1.0 - t) |*| Vec3D 1.0 1.0 1.0) + (t |*| Vec3D 0.5 0.7 1.0))


type Radius = Double
-- Determine whether a ray hits the surface of a sphere with a
-- `radius` and `center`. It returns the timestep for the ray at
-- which this occurs.
hitsSphere :: Point -> Radius -> Ray -> Maybe Double
hitsSphere center radius ray =
    if discriminant < 0
        then Nothing
        else Just $ (-b - (sqrt discriminant)) / (2.0 * a)
    where discriminant = b ** 2 - 4 * a * c
          a = dotProduct (direction ray) (direction ray)
          b = 2.0 * (dotProduct oc $ direction ray)
          c = (dotProduct oc oc) - (radius * radius)
          oc = (source ray) - center

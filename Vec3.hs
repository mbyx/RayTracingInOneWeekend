module Vec3 where

type Color = Vec3
type Point = Vec3

data Vec3 = Vec3 Double Double Double

instance Show Vec3 where
  show (Vec3 r g b) = "(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

defaultVec3 :: Vec3
defaultVec3 = Vec3 0 0 0

getX :: Vec3 -> Double
getX (Vec3 x _ _) = x

getY :: Vec3 -> Double
getY (Vec3 _ y _) = y

getZ :: Vec3 -> Double
getZ (Vec3 _ _ z) = z

lenSqr :: Vec3 -> Double
lenSqr (Vec3 x y z) = (x * x) + (y * y) + (z * z)

len :: Vec3 -> Double
len v = sqrt $ lenSqr v

multScalar :: Vec3 -> Double -> Vec3
multScalar (Vec3 x y z) s = Vec3 (x * s) (y * s) (z * s)

divScalar :: Vec3 -> Double -> Vec3
divScalar v s = multScalar v (1 / s)

dotProduct :: Vec3 -> Vec3 -> Double
dotProduct (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    (x1 * x2) +
    (y1 * y2) +
    (z1 * z2)

crossProduct :: Vec3 -> Vec3 -> Vec3
crossProduct (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    Vec3 ((y1 * z2) - (z1 * y2))
         ((z1 * x2) - (x1 * z2))
         ((x1 * y2) - (y1 * x2))

unit :: Vec3 -> Vec3
unit v = divScalar v $ len v

instance Num Vec3 where
    (Vec3 x1 y1 z1) + (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
    (Vec3 x1 y1 z1) - (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
    (Vec3 x1 y1 z1) * (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)
    abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
    signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
    fromInteger i = Vec3 (fromInteger i) (fromInteger i) (fromInteger i)

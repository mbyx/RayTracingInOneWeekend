module Util where

-- Haskell, being statically typed is not very ergonomic to work
-- with when you have to do maths. These types work in a generic
-- way such that they take in any number as the lhs, and any fraction
-- as the rhs.

(/.) :: (Integral a, Fractional b, Integral a1) => a -> a1 -> b
x /. y = fromIntegral x / fromIntegral y

(*.) :: (Integral a, Fractional b, Integral a1) => a -> a1 -> b
x *. y = fromIntegral x * fromIntegral y

(+.) :: (Integral a, Fractional b, Integral a1) => a -> a1 -> b
x +. y = fromIntegral x + fromIntegral y

(-.) :: (Integral a, Fractional b, Integral a1) => a -> a1 -> b
x -. y = fromIntegral x - fromIntegral y

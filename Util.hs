module Util where

(/.) :: (Integral a, Fractional b, Integral a1) => a -> a1 -> b
x /. y = fromIntegral x / fromIntegral y

(*.) :: (Integral a, Fractional b, Integral a1) => a -> a1 -> b
x *. y = fromIntegral x * fromIntegral y

(+.) :: (Integral a, Fractional b, Integral a1) => a -> a1 -> b
x +. y = fromIntegral x + fromIntegral y

(-.) :: (Integral a, Fractional b, Integral a1) => a -> a1 -> b
x -. y = fromIntegral x - fromIntegral y

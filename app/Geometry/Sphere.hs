module Geometry.Sphere
  ( volume
  , area
  ) where

volume :: Float -> Float
volume = (* 4.0) . (/ 3) . (* pi) . (^ 3)

area :: Float -> Float
area = (* 4) . (* pi) . (^ 2)

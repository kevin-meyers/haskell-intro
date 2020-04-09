module Vector
  ( vPlus
  , dotProd
  , vMult
  ) where

data Vector a =
  Vector a a a
  deriving (Show)

vPlus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vPlus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

dotProd :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `dotProd` (Vector l m n) = Vector (i * l) (j * m) (k * n)

vMult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vMult` m = Vector (i * m) (j * m) (k * m)

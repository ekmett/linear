module Linear.Epsilon
  ( Epsilon(..)
  ) where

class Num a => Epsilon a where
  nearZero :: a -> Bool

instance Epsilon Float where
  nearZero a = abs a <= 1e-6

instance Epsilon Double where
  nearZero a = abs a <= 1e-12

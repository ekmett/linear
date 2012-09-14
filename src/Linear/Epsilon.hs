module Linear.Epsilon
  ( Epsilon(..)
  ) where

-- | Provides a fairly subjective test to see if a quantity is near zero.
class Num a => Epsilon a where
  nearZero :: a -> Bool

-- | @'abs' a '<=' 1e-6@
instance Epsilon Float where
  nearZero a = abs a <= 1e-6

-- | @'abs' a '<=' 1e-12@
instance Epsilon Double where
  nearZero a = abs a <= 1e-12

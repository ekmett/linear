module Linear.Conjugate
  ( Conjugate(..)
  ) where

import Data.Complex hiding (conjugate)

class Conjugate a where
  conjugate :: a -> a
  conjugate = id

instance Conjugate Double
instance Conjugate Float
instance (Conjugate a, RealFloat a) => Conjugate (Complex a) where
  {-# SPECIALIZE instance Conjugate (Complex Float) #-}
  {-# SPECIALIZE instance Conjugate (Complex Double) #-}
  conjugate (a :+ b) = conjugate a :+ negate b

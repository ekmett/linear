{-# LANGUAGE DefaultSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Testing for values "near" zero
-----------------------------------------------------------------------------
module Linear.Epsilon
  ( MkEpsilon(..)
  , Epsilon(..)
  , aboutEqual
  ) where
import Data.Complex (Complex, magnitude)
import Foreign.C.Types (CFloat, CDouble)

-- | Provides a near-zero quantity
class MkEpsilon a where
  -- | A near-zero quantity
  epsilon :: a

-- | ε = 1e-6
instance MkEpsilon Float where
  epsilon = 1e-6

-- | ε = 1e-12
instance MkEpsilon Double where
  epsilon = 1e-12

-- | ε = 1e-6
instance MkEpsilon CFloat where
  epsilon = 1e-6

-- | ε = 1e-12
instance MkEpsilon CDouble where
  epsilon = 1e-12

-- | Provides a fairly subjective test to see if a quantity is near zero.
--
-- >>> nearZero (1e-11 :: Double)
-- False
--
-- >>> nearZero (1e-17 :: Double)
-- True
--
-- >>> nearZero (1e-5 :: Float)
-- False
--
-- >>> nearZero (1e-7 :: Float)
-- True
class Num a => Epsilon a where
  -- | Determine if a quantity is near zero.
  nearZero :: a -> Bool
  default nearZero :: (MkEpsilon a, Ord a) => a -> Bool
  nearZero a = abs a <= epsilon

-- | Equality up to epsilon
--
-- @'aboutEqual' a b@ is true iff @|a - b| < ε@
aboutEqual :: Epsilon a => a -> a -> Bool
aboutEqual a b = nearZero (a - b)

-- | @'abs' a '<=' 1e-6@
instance Epsilon Float where

-- | @'abs' a '<=' 1e-12@
instance Epsilon Double where

-- | @'abs' a '<=' 1e-6@
instance Epsilon CFloat where

-- | @'abs' a '<=' 1e-12@
instance Epsilon CDouble where

instance (Epsilon a, RealFloat a) => Epsilon (Complex a) where
  nearZero = nearZero . magnitude

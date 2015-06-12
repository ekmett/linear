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
  ( Epsilon(..)
  ) where
import Foreign.C.Types (CFloat, CDouble)

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

-- | @'abs' a '<=' 1e-6@
instance Epsilon Float where
  nearZero a = abs a <= 1e-6

-- | @'abs' a '<=' 1e-12@
instance Epsilon Double where
  nearZero a = abs a <= 1e-12

-- | @'abs' a '<=' 1e-6@
instance Epsilon CFloat where
  nearZero a = abs a <= 1e-6

-- | @'abs' a '<=' 1e-12@
instance Epsilon CDouble where
  nearZero a = abs a <= 1e-12

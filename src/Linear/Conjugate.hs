{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Linear.Conjugate
-- Copyright   :  (C) 2012-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Involutive rings
----------------------------------------------------------------------------
module Linear.Conjugate
  ( Conjugate(..)
  ) where

import Data.Complex hiding (conjugate)
import Data.Int
import Data.Word
import Foreign.C.Types (CFloat, CDouble)

-- | An involutive ring
class Num a => Conjugate a where
  -- | Conjugate a value. This defaults to the trivial involution.
  --
  -- >>> conjugate (1 :+ 2)
  -- 1.0 :+ (-2.0)
  --
  -- >>> conjugate 1
  -- 1
  conjugate :: a -> a
  conjugate = id

instance Conjugate Integer
instance Conjugate Int
instance Conjugate Int64
instance Conjugate Int32
instance Conjugate Int16
instance Conjugate Int8
instance Conjugate Word
instance Conjugate Word64
instance Conjugate Word32
instance Conjugate Word16
instance Conjugate Word8
instance Conjugate Double
instance Conjugate Float
instance Conjugate CFloat
instance Conjugate CDouble

instance (Conjugate a, RealFloat a) => Conjugate (Complex a) where
  {-# SPECIALIZE instance Conjugate (Complex Float) #-}
  {-# SPECIALIZE instance Conjugate (Complex Double) #-}
  conjugate (a :+ b) = conjugate a :+ negate b

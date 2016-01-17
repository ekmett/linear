{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-} -- *sigh* TrivialConjugate isn't redundant, GHC
#endif

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-2015 Edward Kmett
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
  , TrivialConjugate
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
#ifndef HLINT
  default conjugate :: TrivialConjugate a => a -> a
  conjugate = id
#endif

-- | Requires and provides a default definition such that
--
-- @
-- 'conjugate' = 'id'
-- @
class Conjugate a => TrivialConjugate a

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

instance TrivialConjugate Integer
instance TrivialConjugate Int
instance TrivialConjugate Int64
instance TrivialConjugate Int32
instance TrivialConjugate Int16
instance TrivialConjugate Int8
instance TrivialConjugate Word
instance TrivialConjugate Word64
instance TrivialConjugate Word32
instance TrivialConjugate Word16
instance TrivialConjugate Word8
instance TrivialConjugate Double
instance TrivialConjugate Float
instance TrivialConjugate CFloat
instance TrivialConjugate CDouble

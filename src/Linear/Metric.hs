{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Linear.Metric
-- Copyright   :  (C) 2012-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Free metric spaces
----------------------------------------------------------------------------
module Linear.Metric
  ( Metric(..), normalize
  ) where

import Data.Foldable as Foldable
import Data.Functor.Identity
import Data.Vector (Vector)
import Linear.Epsilon
import Linear.Vector

-- $setup
-- >>> import Linear

-- | Free and sparse inner product/metric spaces.
class Additive f => Metric f where
  -- | Compute the inner product of two vectors or (equivalently)
  -- convert a vector @f a@ into a covector @f a -> a@.
  --
  -- >>> V2 1 2 `dot` V2 3 4
  -- 11
  dot :: Num a => f a -> f a -> a
#ifndef HLINT
  default dot :: (Foldable f, Num a) => f a -> f a -> a
  dot x y = Foldable.sum $ liftI2 (*) x y
#endif

  -- | Compute the squared norm. The name quadrance arises from
  -- Norman J. Wildberger's rational trigonometry.
  quadrance :: Num a => f a -> a
  quadrance v = dot v v

  -- | Compute the quadrance of the difference
  qd :: Num a => f a -> f a -> a
  qd f g = quadrance (f ^-^ g)

  -- | Compute the distance between two vectors in a metric space
  distance :: Floating a => f a -> f a -> a
  distance f g = norm (f ^-^ g)

  -- | Compute the norm of a vector in a metric space
  norm :: Floating a => f a -> a
  norm v = sqrt (quadrance v)

  -- | Convert a non-zero vector to unit vector.
  signorm :: Floating a => f a -> f a
  signorm v = fmap (/m) v where
    m = norm v

instance Metric Identity where
  dot (Identity x) (Identity y) = x * y

instance Metric Vector

-- | Normalize a 'Metric' functor to have unit 'norm'. This function
-- does not change the functor if its 'norm' is 0 or 1.
normalize :: (Floating a, Metric f, Epsilon a) => f a -> f a
normalize v = if nearZero l || nearZero (1-l) then v else fmap (/sqrt l) v
  where l = quadrance v

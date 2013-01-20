{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Linear.Additive
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Operations on free vector spaces.
-----------------------------------------------------------------------------
module Linear.Additive
  ( Additive(..)
  ) where

import Control.Applicative
import Data.Complex
import Data.Foldable (foldMap)
import Data.Functor.Bind
import Data.Functor.Identity
import Data.HashMap.Lazy as HashMap
import Data.Hashable
import Data.IntMap as IntMap
import Data.Map as Map
import Data.Monoid (Sum(..))
import Data.Traversable (Traversable, mapAccumL)
import Linear.Instances ()

-- $setup
-- >>> import Control.Lens
-- >>> import Linear.V2

infixl 6 ^+^, ^-^

-- | A vector is an additive group with additional structure.
class Bind f => Additive f where
  -- | The zero vector
  zero :: Num a => f a
#ifndef HLINT
  default zero :: (Applicative f, Num a) => f a
  zero = pure 0
#endif

  -- | Compute the sum of two vectors
  --
  -- >>> V2 1 2 ^+^ V2 3 4
  -- V2 4 6
  (^+^) :: Num a => f a -> f a -> f a
#ifndef HLINT
  default (^+^) :: (Applicative f, Num a) => f a -> f a -> f a
  (^+^) = liftA2 (+)
  {-# INLINE (^+^) #-}
#endif

  -- | Compute the difference between two vectors
  --
  -- >>> V2 4 5 - V2 3 1
  -- V2 1 4
  (^-^) :: Num a => f a -> f a -> f a
#ifndef HLINT
  default (^-^) :: (Applicative f, Num a) => f a -> f a -> f a
  (^-^) = liftA2 (-)
  {-# INLINE (^-^) #-}
#endif

  -- | Linearly interpolate between two vectors.
  lerp :: Num a => a -> f a -> f a -> f a
  lerp alpha u v = alpha *^ u ^+^ (1 - alpha) *^ v
  {-# INLINE lerp #-}

instance Additive IntMap where
  zero = IntMap.empty
  (^+^) = IntMap.unionWith (+)
  xs ^-^ ys = IntMap.unionWith (+) xs (negated ys)

instance Ord k => Additive (Map k) where
  zero = Map.empty
  (^+^) = Map.unionWith (+)
  xs ^-^ ys = Map.unionWith (+) xs (negated ys)

instance (Eq k, Hashable k) => Additive (HashMap k) where
  zero = HashMap.empty
  (^+^) = HashMap.unionWith (+)
  xs ^-^ ys = HashMap.unionWith (+) xs (negated ys)

instance Additive ((->) b)

instance Additive Complex

instance Additive Identity


{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Linear.V0
-- Copyright   :  (C) 2012-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- 2-D Vectors
----------------------------------------------------------------------------
module Linear.V0
  ( V0(..)
  ) where

import Control.Applicative
import Data.Data
import Data.Distributive
import Data.Foldable
import Data.Ix
import Data.Traversable
import Data.Semigroup
import Data.Functor.Bind
import Foreign.Storable (Storable(..))
import Linear.Core
import Linear.Metric
import Linear.Epsilon
import Linear.Vector
import Prelude hiding (sum)

-- $setup
-- >>> import Control.Lens

-- | A 0-dimensional vector
--
-- >>> pure 1 :: V0 Int
-- V0
--
-- >>> V0 + V0
-- V0
--
data V0 a = V0 deriving (Eq,Ord,Show,Read,Ix,Enum,Data,Typeable)

instance Functor V0 where
  fmap _ V0 = V0
  {-# INLINE fmap #-}
  _ <$ _ = V0
  {-# INLINE (<$) #-}

instance Foldable V0 where
  foldMap _ V0 = mempty
  {-# INLINE foldMap #-}

instance Traversable V0 where
  traverse _ V0 = pure V0
  {-# INLINE traverse #-}

instance Apply V0 where
  V0 <.> V0 = V0
  {-@ INLINE (<.>) #-}

instance Applicative V0 where
  pure _ = V0
  {-# INLINE pure #-}
  V0 <*> V0 = V0
  {-@ INLINE (<*>) #-}

instance Additive V0

instance Bind V0 where
  V0 >>- _ = V0
  {-# INLINE (>>-) #-}

instance Monad V0 where
  return _ = V0
  {-# INLINE return #-}
  V0 >>= _ = V0
  {-# INLINE (>>=) #-}

instance Num (V0 a) where
  V0 + V0 = V0
  {-# INLINE (+) #-}
  V0 - V0 = V0
  {-# INLINE (-) #-}
  V0 * V0 = V0
  {-# INLINE (*) #-}
  negate V0 = V0
  {-# INLINE negate #-}
  abs V0 = V0
  {-# INLINE abs #-}
  signum V0 = V0
  {-# INLINE signum #-}
  fromInteger _ = V0
  {-# INLINE fromInteger #-}

instance Fractional (V0 a) where
  recip _ = V0
  {-# INLINE recip #-}
  V0 / V0 = V0
  {-# INLINE (/) #-}
  fromRational _ = V0
  {-# INLINE fromRational #-}

instance Metric V0 where
  dot V0 V0 = 0
  {-# INLINE dot #-}

instance Core V0 where
  core _ = V0
  {-# INLINE core #-}

instance Distributive V0 where
  distribute _ = V0
  {-# INLINE distribute #-}

instance Epsilon a => Epsilon (V0 a) where
  nearZero _ = True
  {-# INLINE nearZero #-}

instance Storable a => Storable (V0 a) where
  sizeOf _ = 0
  {-# INLINE sizeOf #-}
  alignment _ = 1
  {-# INLINE alignment #-}
  poke _ V0 = return ()
  {-# INLINE poke #-}
  peek _ = return V0
  {-# INLINE peek #-}

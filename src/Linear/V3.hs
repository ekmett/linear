{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Linear.V3
-- Copyright   :  (C) 2012-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- 3-D Vectors
----------------------------------------------------------------------------
module Linear.V3
  ( V3(..)
  , cross, triple
  , R2(..)
  , R3(..)
  ) where

import Control.Applicative
import Data.Data
import Data.Distributive
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import GHC.Arr (Ix(..))
import Linear.Core
import Linear.Epsilon
import Linear.Metric
import Linear.V2

-- | A 3-dimensional vector
data V3 a = V3 a a a deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Functor V3 where
  fmap f (V3 a b c) = V3 (f a) (f b) (f c)
  {-# INLINE fmap #-}
  a <$ _ = V3 a a a
  {-# INLINE (<$) #-}

instance Foldable V3 where
  foldMap f (V3 a b c) = f a `mappend` f b `mappend` f c
  {-# INLINE foldMap #-}

instance Traversable V3 where
  traverse f (V3 a b c) = V3 <$> f a <*> f b <*> f c
  {-# INLINE traverse #-}

instance Applicative V3 where
  pure a = V3 a a a
  {-# INLINE pure #-}
  V3 a b c <*> V3 d e f = V3 (a d) (b e) (c f)
  {-# INLINE (<*>) #-}

instance Monad V3 where
  return a = V3 a a a
  {-# INLINE return #-}
  V3 a b c >>= f = V3 a' b' c' where
    V3 a' _ _ = f a
    V3 _ b' _ = f b
    V3 _ _ c' = f c
  {-# INLINE (>>=) #-}

instance Num a => Num (V3 a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (V3 a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Metric V3 where
  dot (V3 a b c) (V3 d e f) = a * d + b * e + c * f
  {-# INLINABLE dot #-}

instance Distributive V3 where
  distribute f = V3 (fmap (\(V3 x _ _) -> x) f) (fmap (\(V3 _ y _) -> y) f) (fmap (\(V3 _ _ z) -> z) f)
  {-# INLINE distribute #-}

-- | A space that distinguishes 3 orthogonal basis vectors: '_x', '_y', and '_z'. (It may have more)
class R2 t => R3 t where
  _z :: Functor f => (a -> f a) -> t a -> f (t a)
  _xyz :: Functor f => (V3 a -> f (V3 a)) -> t a -> f (t a)

instance R2 V3 where
  _x f (V3 a b c) = (\a' -> V3 a' b c) <$> f a
  {-# INLINE _x #-}
  _y f (V3 a b c) = (\b' -> V3 a b' c) <$> f b
  {-# INLINE _y #-}
  _xy f (V3 a b c) = (\(V2 a' b') -> V3 a' b' c) <$> f (V2 a b)
  {-# INLINE _xy #-}

instance R3 V3 where
  _z f (V3 a b c) = V3 a b <$> f c
  {-# INLINE _z #-}
  _xyz = id
  {-# INLINE _xyz #-}

instance Core V3 where
  core f = V3 (f _x) (f _y) (f _z)
  {-# INLINE core #-}

instance forall a. Storable a => Storable (V3 a) where
  sizeOf _ = 3 * sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (V3 x y z) = do poke ptr' x
                           pokeElemOff ptr' 1 y
                           pokeElemOff ptr' 2 z
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = V3 <$> peek ptr' <*> peekElemOff ptr' 1 <*> peekElemOff ptr' 2
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

-- | cross product
cross :: Num a => V3 a -> V3 a -> V3 a
cross (V3 a b c) (V3 d e f) = V3 (b*f-c*e) (c*d-a*f) (a*e-b*d)
{-# INLINABLE cross #-}

-- | scalar triple product
triple :: Num a => V3 a -> V3 a -> V3 a -> a
triple a b c = dot a (cross b c)
{-# INLINE triple #-}

instance Epsilon a => Epsilon (V3 a) where
  nearZero = nearZero . quadrance
  {-# INLINE nearZero #-}

instance Ix a => Ix (V3 a) where
  {-# SPECIALISE instance Ix (V3 Int) #-}

  range (V3 l1 l2 l3,V3 u1 u2 u3) =
      [V3 i1 i2 i3 | i1 <- range (l1,u1)
                   , i2 <- range (l2,u2)
                   , i3 <- range (l3,u3)
                   ]
  {-# INLINE range #-}

  unsafeIndex (V3 l1 l2 l3,V3 u1 u2 u3) (V3 i1 i2 i3) =
    unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
    unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
    unsafeIndex (l1,u1) i1))
  {-# INLINE unsafeIndex #-}

  inRange (V3 l1 l2 l3,V3 u1 u2 u3) (V3 i1 i2 i3) =
    inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
    inRange (l3,u3) i3
  {-# INLINE inRange #-}

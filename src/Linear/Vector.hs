{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Linear.Vector
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Operations on free vector spaces.
-----------------------------------------------------------------------------
module Linear.Vector
  ( Additive(..)
  , negated
  , (^*)
  , (*^)
  , (^/)
  , basis
  , basisFor
  ) where

import Control.Applicative
import Data.Foldable (foldMap)
import Data.Functor.Bind
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
infixl 7 ^*, *^, ^/

class Bind f => Additive f where
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
  (^+^) = IntMap.unionWith (+)
  xs ^-^ ys = IntMap.unionWith (+) xs (negated ys)

instance Ord k => Additive (Map k) where
  (^+^) = Map.unionWith (+)
  xs ^-^ ys = Map.unionWith (+) xs (negated ys)

instance (Eq k, Hashable k) => Additive (HashMap k) where
  (^+^) = HashMap.unionWith (+)
  xs ^-^ ys = HashMap.unionWith (+) xs (negated ys)

-- | Compute the negation of a vector
--
-- >>> negated (V2 2 4)
-- V2 (-2) (-4)
negated :: (Functor f, Num a) => f a -> f a
negated = fmap negate
{-# INLINE negated #-}

-- | Compute the left scalar product
--
-- >>> 2 *^ V2 3 4
-- V2 6 8
(*^) :: (Functor f, Num a) => a -> f a -> f a
(*^) a = fmap (a*)
{-# INLINE (*^) #-}

-- | Compute the right scalar product
--
-- >>> V2 3 4 ^* 2
-- V2 6 8
(^*) :: (Functor f, Num a) => f a -> a -> f a
f ^* a = fmap (*a) f
{-# INLINE (^*) #-}

-- | Compute division by a scalar on the right.
(^/) :: (Functor f, Fractional a) => f a -> a -> f a
f ^/ a = fmap (/a) f
{-# INLINE (^/) #-}

-- @setElement i x v@ sets the @i@'th element of @v@ to @x@.
setElement :: Traversable t => Int -> a -> t a -> t a
setElement i x = snd . mapAccumL aux 0
  where aux j y = let j' = j + 1
                      y' = if i == j then x else y
                  in j' `seq` (j', y')

-- | Produce a default basis for a vector space. If the dimensionality
-- of the vector space is not statically known, see 'basisFor'.
basis :: (Applicative t, Traversable t, Num a) => [t a]
basis = [ setElement k 1 zero | k <- [0..n - 1] ]
  where zero = pure 0
        n = getSum $ foldMap (const (Sum 1)) zero

-- | Produce a default basis for a vector space from which the
-- argument is drawn.
basisFor :: (Traversable t, Enum a, Num a) => t a -> [t a]
basisFor v = [ setElement k 1 z | k <- [0..n-1] ]
  where z = 0 <$ v
        n = getSum $ foldMap (const (Sum 1)) v

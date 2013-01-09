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
  ( (^+^)
  , gnegate
  , (^-^)
  , (^*)
  , (*^)
  , (^/)
  , lerp
  -- , basis
  -- , basisFor
  ) where

import Control.Applicative

infixl 6 ^+^, ^-^
infixl 7 ^*, *^, ^/

-- | Compute the sum of two vectors
(^+^) :: (Applicative f, Num a) => f a -> f a -> f a
(^+^) = liftA2 (+)
{-# INLINE (^+^) #-}

-- | Compute the negation of a vector
gnegate :: (Functor f, Num a) => f a -> f a
gnegate = fmap negate
{-# INLINE gnegate #-}

-- | Compute the difference between two vectors
(^-^) :: (Applicative f, Num a) => f a -> f a -> f a
(^-^) = liftA2 (-)
{-# INLINE (^-^) #-}

-- | Compute the left scalar product
(*^) :: (Functor f, Num a) => a -> f a -> f a
(*^) a = fmap (a*)
{-# INLINE (*^) #-}

-- | Compute the right scalar product
(^*) :: (Functor f, Num a) => f a -> a -> f a
f ^* a = fmap (*a) f
{-# INLINE (^*) #-}

-- | Compute division by a scalar on the right.
(^/) :: (Functor f, Fractional a) => f a -> a -> f a
f ^/ a = fmap (/a) f
{-# INLINE (^/) #-}

-- | Linearly interpolate between two vectors.
lerp :: (Applicative f, Num a) => a -> f a -> f a -> f a
lerp alpha u v = alpha *^ u ^+^ (1 - alpha) *^ v
{-# INLINE lerp #-}

{-

-- | Produce a default basis for a vector space. If the dimensionality
-- of the vector space is not statically known, see 'basisFor'.
basis :: (Applicative t, Traversable t, Num a) => [t a]
basis = [ set (element k) 1 zero | k <- [0..lengthOf folded zero - 1]]
  where zero = pure 0

-- | Produce a default basis for a vector space from which the
-- argument is drawn.
basisFor :: (Traversable t, Enum a, Num a) => t a -> [t a]
basisFor v = map aux [0..n-1]
  where z = 0 <$ v
        n = lengthOf folded z
        aux i = z & element i .~ 1

-}

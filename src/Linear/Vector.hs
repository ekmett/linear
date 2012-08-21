-- | operations on free vector spaces
module Linear.Vector
  ( (^+^)
  , gnegate
  , (^-^)
  , (^*)
  , (*^)
  , (^/)
  , lerp
  ) where

import Control.Applicative

infixl 6 ^+^, ^-^
infixl 7 ^*, *^, ^/

-- | sum of two vectors
(^+^) :: (Applicative f, Num a) => f a -> f a -> f a
(^+^) = liftA2 (+)
{-# INLINE (^+^) #-}

-- | negation of a vector
gnegate :: (Functor f, Num a) => f a -> f a
gnegate = fmap negate
{-# INLINE gnegate #-}

-- | difference between two vector
(^-^) :: (Applicative f, Num a) => f a -> f a -> f a
(^-^) = liftA2 (-)
{-# INLINE (^-^) #-}

-- | left scalar product
(*^) :: (Functor f, Num a) => a -> f a -> f a
(*^) a = fmap (a*)
{-# INLINE (*^) #-}

-- | right scalar product
(^*) :: (Functor f, Num a) => f a -> a -> f a
f ^* a = fmap (*a) f
{-# INLINE (^*) #-}

(^/) :: (Functor f, Fractional a) => f a -> a -> f a
f ^/ a = fmap (/a) f
{-# INLINE (^/) #-}

-- | linear interpolation
lerp :: (Applicative f, Num a) => a -> f a -> f a -> f a
lerp alpha u v = alpha *^ u ^+^ (1 - alpha) *^ v
{-# INLINE lerp #-}

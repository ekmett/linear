{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
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
  , kronecker
  ) where

import Control.Applicative
import Data.Complex
import Data.Foldable as Foldable (foldMap, forM_)
import Data.Functor.Identity
import Data.HashMap.Lazy as HashMap
import Data.Hashable
import Data.IntMap as IntMap
import Data.Map as Map
import Data.Monoid (Sum(..), mempty)
import Data.Vector as Vector
import Data.Vector.Mutable as Mutable
import Data.Traversable (Traversable, mapAccumL)
import GHC.Generics
import Linear.Instances ()

-- $setup
-- >>> import Control.Lens
-- >>> import Linear.V2

infixl 6 ^+^, ^-^
infixl 7 ^*, *^, ^/

class GAdditive f where
  gzero :: Num a => f a
  gliftU2 :: (a -> a -> a) -> f a -> f a -> f a
  gliftI2 :: (a -> b -> c) -> f a -> f b -> f c

instance GAdditive U1 where
  gzero = U1
  {-# INLINE gzero #-}
  gliftU2 _ U1 U1 = U1
  {-# INLINE gliftU2 #-}
  gliftI2 _ U1 U1 = U1
  {-# INLINE gliftI2 #-}

instance (GAdditive f, GAdditive g) => GAdditive (f :*: g) where
  gzero = gzero :*: gzero
  {-# INLINE gzero #-}
  gliftU2 f (a :*: b) (c :*: d) = gliftU2 f a c :*: gliftU2 f b d
  {-# INLINE gliftU2 #-}
  gliftI2 f (a :*: b) (c :*: d) = gliftI2 f a c :*: gliftI2 f b d
  {-# INLINE gliftI2 #-}

instance Additive f => GAdditive (Rec1 f) where
  gzero = Rec1 zero
  {-# INLINE gzero #-}
  gliftU2 f (Rec1 g) (Rec1 h) = Rec1 (liftU2 f g h)
  {-# INLINE gliftU2 #-}
  gliftI2 f (Rec1 g) (Rec1 h) = Rec1 (liftI2 f g h)
  {-# INLINE gliftI2 #-}

instance GAdditive f => GAdditive (M1 i c f) where
  gzero = M1 gzero
  {-# INLINE gzero #-}
  gliftU2 f (M1 g) (M1 h) = M1 (gliftU2 f g h)
  {-# INLINE gliftU2 #-}
  gliftI2 f (M1 g) (M1 h) = M1 (gliftI2 f g h)
  {-# INLINE gliftI2 #-}

instance GAdditive Par1 where
  gzero = Par1 0
  gliftU2 f (Par1 a) (Par1 b) = Par1 (f a b)
  {-# INLINE gliftU2 #-}
  gliftI2 f (Par1 a) (Par1 b) = Par1 (f a b)
  {-# INLINE gliftI2 #-}


-- | A vector is an additive group with additional structure.
class Functor f => Additive f where
  -- | The zero vector
  zero :: Num a => f a
#ifndef HLINT
  default zero :: (GAdditive (Rep1 f), Generic1 f, Num a) => f a
  zero = to1 gzero
#endif

  -- | Compute the sum of two vectors
  --
  -- >>> V2 1 2 ^+^ V2 3 4
  -- V2 4 6
  (^+^) :: Num a => f a -> f a -> f a
#ifndef HLINT
  default (^+^) :: Num a => f a -> f a -> f a
  (^+^) = liftU2 (+)
  {-# INLINE (^+^) #-}
#endif

  -- | Compute the difference between two vectors
  --
  -- >>> V2 4 5 - V2 3 1
  -- V2 1 4
  (^-^) :: Num a => f a -> f a -> f a
#ifndef HLINT
  default (^-^) :: Num a => f a -> f a -> f a
  x ^-^ y = x ^+^ negated y
  {-# INLINE (^-^) #-}
#endif

  -- | Linearly interpolate between two vectors.
  lerp :: Num a => a -> f a -> f a -> f a
  lerp alpha u v = alpha *^ u ^+^ (1 - alpha) *^ v
  {-# INLINE lerp #-}

  -- | Apply a function to merge the 'non-zero' components of two vectors, unioning the rest of the values.
  --
  -- * For a dense vector this is equivalent to 'liftA2'.
  --
  -- * For a sparse vector this is equivalent to 'unionWith'.
  liftU2 :: (a -> a -> a) -> f a -> f a -> f a
#ifndef HLINT
  default liftU2 :: Applicative f => (a -> a -> a) -> f a -> f a -> f a
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
#endif

  -- | Apply a function to the components of two vectors.
  --
  -- * For a dense vector this is equivalent to 'liftA2'.
  --
  -- * For a sparse vector this is equivalent to 'intersectionWith'.
  liftI2 :: (a -> b -> c) -> f a -> f b -> f c
#ifndef HLINT
  default liftI2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
  liftI2 = liftA2
  {-# INLINE liftI2 #-}
#endif

instance Additive ZipList where
  zero = ZipList []
  {-# INLINE zero #-}
  liftU2 f (ZipList xs) (ZipList ys) = ZipList (liftU2 f xs ys)
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

instance Additive Vector where
  zero = mempty
  {-# INLINE zero #-}
  liftU2 f u v = case compare lu lv of
    LT | lu == 0   -> v
       | otherwise -> modify (\ w -> Foldable.forM_ [0..lu-1] $ \i -> unsafeWrite w i $ f (unsafeIndex u i) (unsafeIndex v i)) v
    EQ -> Vector.zipWith f u v
    GT | lv == 0   -> u
       | otherwise -> modify (\ w -> Foldable.forM_ [0..lv-1] $ \i -> unsafeWrite w i $ f (unsafeIndex u i) (unsafeIndex v i)) u
    where
      lu = Vector.length u
      lv = Vector.length v
  {-# INLINE liftU2 #-}
  liftI2 = Vector.zipWith
  {-# INLINE liftI2 #-}

instance Additive Maybe where
  zero = Nothing
  {-# INLINE zero #-}
  liftU2 f (Just a) (Just b) = Just (f a b)
  liftU2 _ Nothing ys = ys
  liftU2 _ xs Nothing = xs
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

instance Additive [] where
  zero = []
  {-# INLINE zero #-}
  liftU2 f = go where
    go (x:xs) (y:ys) = f x y : go xs ys
    go [] ys = ys
    go xs [] = xs
  {-# INLINE liftU2 #-}
  liftI2 = Prelude.zipWith
  {-# INLINE liftI2 #-}

instance Additive IntMap where
  zero = IntMap.empty
  {-# INLINE zero #-}
  liftU2 = IntMap.unionWith
  {-# INLINE liftU2 #-}
  liftI2 = IntMap.intersectionWith
  {-# INLINE liftI2 #-}

instance Ord k => Additive (Map k) where
  zero = Map.empty
  {-# INLINE zero #-}
  liftU2 = Map.unionWith
  {-# INLINE liftU2 #-}
  liftI2 = Map.intersectionWith
  {-# INLINE liftI2 #-}

instance (Eq k, Hashable k) => Additive (HashMap k) where
  zero = HashMap.empty
  {-# INLINE zero #-}
  liftU2 = HashMap.unionWith
  {-# INLINE liftU2 #-}
  liftI2 = HashMap.intersectionWith
  {-# INLINE liftI2 #-}

instance Additive ((->) b) where
  zero   = const 0
  {-# INLINE zero #-}
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

instance Additive Complex where
  zero = 0 :+ 0
  {-# INLINE zero #-}
  liftU2 f (a :+ b) (c :+ d) = f a c :+ f b d
  {-# INLINE liftU2 #-}
  liftI2 f (a :+ b) (c :+ d) = f a c :+ f b d
  {-# INLINE liftI2 #-}

instance Additive Identity where
  zero = Identity 0
  {-# INLINE zero #-}
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

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
basis = [ setElement k 1 z | k <- [0..n - 1] ]
  where z = pure 0
        n = getSum $ foldMap (const (Sum 1)) z

-- | Produce a default basis for a vector space from which the
-- argument is drawn.
basisFor :: (Traversable t, Enum a, Num a) => t a -> [t a]
basisFor v = [ setElement k 1 z | k <- [0..n-1] ]
  where z = 0 <$ v
        n = getSum $ foldMap (const (Sum 1)) v

-- | Produce a diagonal matrix from a vector.
kronecker :: (Applicative t, Num a, Traversable t) => t a -> t (t a)
kronecker v = snd $ mapAccumL aux 0 v
  where aux i e = let i' = i + 1
                  in i' `seq` (i', setElement i e z)
        z = pure 0

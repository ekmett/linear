{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- 2-D Vectors
----------------------------------------------------------------------------
module Linear.V2
  ( V2(..)
  , R1(..)
  , R2(..)
  , ex, ey
  , perp
  ) where

import Control.Applicative
import Control.Monad (liftM)
import Control.Lens hiding ((<.>))
import Data.Data
import Data.Distributive
import Data.Foldable
import Data.Functor.Bind
import Data.Functor.Rep
import Data.Semigroup
import Data.Semigroup.Foldable
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import GHC.Arr (Ix(..))
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
#endif
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U
import Linear.Metric
import Linear.Epsilon
import Linear.Vector
import Linear.V1 (R1(..),ex)
import Prelude hiding (sum)

-- $setup
-- >>> import Control.Lens

-- | A 2-dimensional vector
--
-- >>> pure 1 :: V2 Int
-- V2 1 1
--
-- >>> V2 1 2 + V2 3 4
-- V2 4 6
--
-- >>> V2 1 2 * V2 3 4
-- V2 3 8
--
-- >>> sum (V2 1 2)
-- 3

data V2 a = V2 !a !a deriving (Eq,Ord,Show,Read,Data,Typeable
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
                              ,Generic
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
                              ,Generic1
#endif
                              )

instance Functor V2 where
  fmap f (V2 a b) = V2 (f a) (f b)
  {-# INLINE fmap #-}
  a <$ _ = V2 a a
  {-# INLINE (<$) #-}

instance Foldable V2 where
  foldMap f (V2 a b) = f a `mappend` f b
  {-# INLINE foldMap #-}

instance Traversable V2 where
  traverse f (V2 a b) = V2 <$> f a <*> f b
  {-# INLINE traverse #-}

instance Foldable1 V2 where
  foldMap1 f (V2 a b) = f a <> f b
  {-# INLINE foldMap1 #-}

instance Traversable1 V2 where
  traverse1 f (V2 a b) = V2 <$> f a <.> f b
  {-# INLINE traverse1 #-}

instance Apply V2 where
  V2 a b <.> V2 d e = V2 (a d) (b e)
  {-@ INLINE (<.>) #-}

instance Applicative V2 where
  pure a = V2 a a
  {-# INLINE pure #-}
  V2 a b <*> V2 d e = V2 (a d) (b e)
  {-@ INLINE (<*>) #-}

instance Additive V2 where
  zero = pure 0
  {-# INLINE zero #-}
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

instance Bind V2 where
  V2 a b >>- f = V2 a' b' where
    V2 a' _ = f a
    V2 _ b' = f b
  {-# INLINE (>>-) #-}

instance Monad V2 where
  return a = V2 a a
  {-# INLINE return #-}
  V2 a b >>= f = V2 a' b' where
    V2 a' _ = f a
    V2 _ b' = f b
  {-# INLINE (>>=) #-}

instance Num a => Num (V2 a) where
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

instance Fractional a => Fractional (V2 a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Metric V2 where
  dot (V2 a b) (V2 c d) = a * c + b * d
  {-# INLINE dot #-}

-- | A space that distinguishes 2 orthogonal basis vectors '_x' and '_y', but may have more.
class R1 t => R2 t where
  -- |
  -- >>> V2 1 2 ^._y
  -- 2
  --
  -- >>> V2 1 2 & _y .~ 3
  -- V2 1 3
  --
  -- @
  -- '_y' :: Lens' (t a) a
  -- @
  _y :: Functor f => (a -> f a) -> t a -> f (t a)
  _y = _xy._y
  {-# INLINE _y #-}

  -- |
  -- @
  -- '_xy' :: Lens' (t a) ('V2' a)
  -- @
  _xy :: Functor f => (V2 a -> f (V2 a)) -> t a -> f (t a)

ey :: R2 t => E t
ey = E _y

instance R1 V2 where
  _x f (V2 a b) = (`V2` b) <$> f a
  {-# INLINE _x #-}

instance R2 V2 where
  _y f (V2 a b) = V2 a <$> f b
  {-# INLINE _y #-}
  _xy = id
  {-# INLINE _xy #-}

instance Distributive V2 where
  distribute f = V2 (fmap (\(V2 x _) -> x) f) (fmap (\(V2 _ y) -> y) f)
  {-# INLINE distribute #-}

-- | the counter-clockwise perpendicular vector
--
-- >>> perp $ V2 10 20
-- V2 (-20) 10
perp :: Num a => V2 a -> V2 a
perp (V2 a b) = V2 (negate b) a
{-# INLINE perp #-}

instance Epsilon a => Epsilon (V2 a) where
  nearZero = nearZero . quadrance
  {-# INLINE nearZero #-}

instance Storable a => Storable (V2 a) where
  sizeOf _ = 2 * sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (V2 x y) = poke ptr' x >> pokeElemOff ptr' 1 y
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = V2 <$> peek ptr' <*> peekElemOff ptr' 1
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Ix a => Ix (V2 a) where
  {-# SPECIALISE instance Ix (V2 Int) #-}

  range (V2 l1 l2,V2 u1 u2) =
    [ V2 i1 i2 | i1 <- range (l1,u1), i2 <- range (l2,u2) ]
  {-# INLINE range #-}

  unsafeIndex (V2 l1 l2,V2 u1 u2) (V2 i1 i2) =
    unsafeIndex (l1,u1) i1 * unsafeRangeSize (l2,u2) + unsafeIndex (l2,u2) i2
  {-# INLINE unsafeIndex #-}

  inRange (V2 l1 l2,V2 u1 u2) (V2 i1 i2) =
    inRange (l1,u1) i1 && inRange (l2,u2) i2
  {-# INLINE inRange #-}

instance Representable V2 where
  type Rep V2 = E V2
  tabulate f = V2 (f ex) (f ey)
  {-# INLINE tabulate #-}
  index xs (E l) = view l xs
  {-# INLINE index #-}

instance FunctorWithIndex (E V2) V2 where
  imap f (V2 a b) = V2 (f ex a) (f ey b)
  {-# INLINE imap #-}

instance FoldableWithIndex (E V2) V2 where
  ifoldMap f (V2 a b) = f ex a `mappend` f ey b
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex (E V2) V2 where
  itraverse f (V2 a b) = V2 <$> f ex a <*> f ey b
  {-# INLINE itraverse #-}

type instance Index (V2 a) = E V2
type instance IxValue (V2 a) = a

instance Ixed (V2 a) where
  ix = el
  {-# INLINE ix #-}

instance Each (V2 a) (V2 b) a b where
  each = traverse
  {-# INLINE each #-}

data instance U.Vector    (V2 a) =  V_V2 !Int (U.Vector    a)
data instance U.MVector s (V2 a) = MV_V2 !Int (U.MVector s a)
instance U.Unbox a => U.Unbox (V2 a)

instance U.Unbox a => M.MVector U.MVector (V2 a) where
  basicLength (MV_V2 n _) = n
  basicUnsafeSlice m n (MV_V2 _ v) = MV_V2 n (M.basicUnsafeSlice (2*m) (2*n) v)
  basicOverlaps (MV_V2 _ v) (MV_V2 _ u) = M.basicOverlaps v u
  basicUnsafeNew n = liftM (MV_V2 n) (M.basicUnsafeNew (2*n))
  basicUnsafeRead (MV_V2 _ v) i =
    do let o = 2*i
       x <- M.basicUnsafeRead v o
       y <- M.basicUnsafeRead v (o+1)
       return (V2 x y)
  basicUnsafeWrite (MV_V2 _ v) i (V2 x y) =
    do let o = 2*i
       M.basicUnsafeWrite v o     x
       M.basicUnsafeWrite v (o+1) y

instance U.Unbox a => G.Vector U.Vector (V2 a) where
  basicUnsafeFreeze (MV_V2 n v) = liftM ( V_V2 n) (G.basicUnsafeFreeze v)
  basicUnsafeThaw   ( V_V2 n v) = liftM (MV_V2 n) (G.basicUnsafeThaw   v)
  basicLength       ( V_V2 n _) = n
  basicUnsafeSlice m n (V_V2 _ v) = V_V2 n (G.basicUnsafeSlice (2*m) (2*n) v)
  basicUnsafeIndexM (V_V2 _ v) i =
    do let o = 2*i
       x <- G.basicUnsafeIndexM v o
       y <- G.basicUnsafeIndexM v (o+1)
       return (V2 x y)

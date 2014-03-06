{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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
-- 3-D Vectors
----------------------------------------------------------------------------
module Linear.V3
  ( V3(..)
  , cross, triple
  , R1(..)
  , R2(..)
  , R3(..)
  , ex, ey, ez
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
import Linear.Epsilon
import Linear.Metric
import Linear.V2
import Linear.Vector

{-# ANN module "HLint: ignore Reduce duplication" #-}

-- | A 3-dimensional vector
data V3 a = V3 !a !a !a deriving (Eq,Ord,Show,Read,Data,Typeable
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
                                 ,Generic
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
                                 ,Generic1
#endif
                                 )


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

instance Foldable1 V3 where
  foldMap1 f (V3 a b c) = f a <> f b <> f c
  {-# INLINE foldMap1 #-}

instance Traversable1 V3 where
  traverse1 f (V3 a b c) = V3 <$> f a <.> f b <.> f c
  {-# INLINE traverse1 #-}

instance Apply V3 where
  V3 a b c <.> V3 d e f = V3 (a d) (b e) (c f)
  {-# INLINE (<.>) #-}

instance Applicative V3 where
  pure a = V3 a a a
  {-# INLINE pure #-}
  V3 a b c <*> V3 d e f = V3 (a d) (b e) (c f)
  {-# INLINE (<*>) #-}

instance Additive V3 where
  zero = pure 0
  {-# INLINE zero #-}
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

instance Bind V3 where
  V3 a b c >>- f = V3 a' b' c' where
    V3 a' _ _ = f a
    V3 _ b' _ = f b
    V3 _ _ c' = f c
  {-# INLINE (>>-) #-}

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
  -- |
  -- @
  -- '_z' :: Lens' (t a) a
  -- @
  _z :: Functor f => (a -> f a) -> t a -> f (t a)
  -- |
  -- @
  -- '_xyz' :: Lens' (t a) ('V3' a)
  -- @
  _xyz :: Functor f => (V3 a -> f (V3 a)) -> t a -> f (t a)

ez :: R3 t => E t
ez = E _z

instance R1 V3 where
  _x f (V3 a b c) = (\a' -> V3 a' b c) <$> f a
  {-# INLINE _x #-}

instance R2 V3 where
  _y f (V3 a b c) = (\b' -> V3 a b' c) <$> f b
  {-# INLINE _y #-}
  _xy f (V3 a b c) = (\(V2 a' b') -> V3 a' b' c) <$> f (V2 a b)
  {-# INLINE _xy #-}

instance R3 V3 where
  _z f (V3 a b c) = V3 a b <$> f c
  {-# INLINE _z #-}
  _xyz = id
  {-# INLINE _xyz #-}

instance Storable a => Storable (V3 a) where
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
    unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) *
    unsafeIndex (l1,u1) i1)
  {-# INLINE unsafeIndex #-}

  inRange (V3 l1 l2 l3,V3 u1 u2 u3) (V3 i1 i2 i3) =
    inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
    inRange (l3,u3) i3
  {-# INLINE inRange #-}

instance Representable V3 where
  type Rep V3 = E V3
  tabulate f = V3 (f ex) (f ey) (f ez)
  {-# INLINE tabulate #-}
  index xs (E l) = view l xs
  {-# INLINE index #-}

instance FunctorWithIndex (E V3) V3 where
  imap f (V3 a b c) = V3 (f ex a) (f ey b) (f ez c)
  {-# INLINE imap #-}

instance FoldableWithIndex (E V3) V3 where
  ifoldMap f (V3 a b c) = f ex a `mappend` f ey b `mappend` f ez c
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex (E V3) V3 where
  itraverse f (V3 a b c) = V3 <$> f ex a <*> f ey b <*> f ez c
  {-# INLINE itraverse #-}

type instance Index (V3 a) = E V3
type instance IxValue (V3 a) = a

#if MIN_VERSION_lens(4,0,0)
instance Ixed (V3 a) where
  ix = el
#else
instance Functor f => Ixed f (V3 a) where
  ix i f = el i (indexed f i)
#endif

data instance U.Vector    (V3 a) =  V_V3 !Int (U.Vector    a)
data instance U.MVector s (V3 a) = MV_V3 !Int (U.MVector s a)
instance U.Unbox a => U.Unbox (V3 a)

instance U.Unbox a => M.MVector U.MVector (V3 a) where
  basicLength (MV_V3 n _) = n
  basicUnsafeSlice m n (MV_V3 _ v) = MV_V3 n (M.basicUnsafeSlice (3*m) (3*n) v)
  basicOverlaps (MV_V3 _ v) (MV_V3 _ u) = M.basicOverlaps v u
  basicUnsafeNew n = liftM (MV_V3 n) (M.basicUnsafeNew (3*n))
  basicUnsafeRead (MV_V3 _ v) i =
    do let o = 3*i
       x <- M.basicUnsafeRead v o
       y <- M.basicUnsafeRead v (o+1)
       z <- M.basicUnsafeRead v (o+2)
       return (V3 x y z)
  basicUnsafeWrite (MV_V3 _ v) i (V3 x y z) =
    do let o = 3*i
       M.basicUnsafeWrite v o     x
       M.basicUnsafeWrite v (o+1) y
       M.basicUnsafeWrite v (o+2) z

instance U.Unbox a => G.Vector U.Vector (V3 a) where
  basicUnsafeFreeze (MV_V3 n v) = liftM ( V_V3 n) (G.basicUnsafeFreeze v)
  basicUnsafeThaw   ( V_V3 n v) = liftM (MV_V3 n) (G.basicUnsafeThaw   v)
  basicLength       ( V_V3 n _) = n
  basicUnsafeSlice m n (V_V3 _ v) = V_V3 n (G.basicUnsafeSlice (3*m) (3*n) v)
  basicUnsafeIndexM (V_V3 _ v) i =
    do let o = 3*i
       x <- G.basicUnsafeIndexM v o
       y <- G.basicUnsafeIndexM v (o+1)
       z <- G.basicUnsafeIndexM v (o+2)
       return (V3 x y z)

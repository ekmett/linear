{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
-- 4-D Vectors
----------------------------------------------------------------------------
module Linear.V4
  ( V4(..)
  , vector, point, normalizePoint
  , R1(..)
  , R2(..)
  , _yx
  , R3(..)
  , _xz, _yz, _zx, _zy
  , _xzy, _yxz, _yzx, _zxy, _zyx
  , R4(..)
  , _xw, _yw, _zw, _wx, _wy, _wz
  , _xyw, _xzw, _xwy, _xwz, _yxw, _yzw, _ywx, _ywz, _zxw, _zyw, _zwx, _zwy
  , _wxy, _wxz, _wyx, _wyz, _wzx, _wzy
  , _xywz, _xzyw, _xzwy, _xwyz, _xwzy, _yxzw , _yxwz, _yzxw, _yzwx, _ywxz
  , _ywzx, _zxyw, _zxwy, _zyxw, _zywx, _zwxy, _zwyx, _wxyz, _wxzy, _wyxz
  , _wyzx, _wzxy, _wzyx
  , ex, ey, ez, ew
  ) where

import Control.Applicative
import Control.Monad (liftM)
import Control.Monad.Fix
import Control.Monad.Zip
import Control.Lens hiding ((<.>))
import Data.Data
import Data.Distributive
import Data.Foldable
import Data.Functor.Bind
import Data.Functor.Rep
import Data.Hashable
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
import Linear.V3
import Linear.Vector

{-# ANN module "HLint: ignore Reduce duplication" #-}

-- | A 4-dimensional vector.
data V4 a = V4 !a !a !a !a deriving (Eq,Ord,Show,Read,Data,Typeable
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
                                    ,Generic
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
                                    ,Generic1
#endif
                                    )

instance Functor V4 where
  fmap f (V4 a b c d) = V4 (f a) (f b) (f c) (f d)
  {-# INLINE fmap #-}
  a <$ _ = V4 a a a a
  {-# INLINE (<$) #-}

instance Foldable V4 where
  foldMap f (V4 a b c d) = f a `mappend` f b `mappend` f c `mappend` f d
  {-# INLINE foldMap #-}

instance Traversable V4 where
  traverse f (V4 a b c d) = V4 <$> f a <*> f b <*> f c <*> f d
  {-# INLINE traverse #-}

instance Foldable1 V4 where
  foldMap1 f (V4 a b c d) = f a <> f b <> f c <> f d
  {-# INLINE foldMap1 #-}

instance Traversable1 V4 where
  traverse1 f (V4 a b c d) = V4 <$> f a <.> f b <.> f c <.> f d
  {-# INLINE traverse1 #-}

instance Applicative V4 where
  pure a = V4 a a a a
  {-# INLINE pure #-}
  V4 a b c d <*> V4 e f g h = V4 (a e) (b f) (c g) (d h)
  {-# INLINE (<*>) #-}

instance Apply V4 where
  V4 a b c d <.> V4 e f g h = V4 (a e) (b f) (c g) (d h)
  {-# INLINE (<.>) #-}

instance Additive V4 where
  zero = pure 0
  {-# INLINE zero #-}
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

instance Bind V4 where
  V4 a b c d >>- f = V4 a' b' c' d' where
    V4 a' _ _ _ = f a
    V4 _ b' _ _ = f b
    V4 _ _ c' _ = f c
    V4 _ _ _ d' = f d
  {-# INLINE (>>-) #-}

instance Monad V4 where
  return a = V4 a a a a
  {-# INLINE return #-}
  V4 a b c d >>= f = V4 a' b' c' d' where
    V4 a' _ _ _ = f a
    V4 _ b' _ _ = f b
    V4 _ _ c' _ = f c
    V4 _ _ _ d' = f d
  {-# INLINE (>>=) #-}

instance Num a => Num (V4 a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (*) = liftA2 (*)
  {-# INLINE (-) #-}
  (-) = liftA2 (-)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (V4 a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Metric V4 where
  dot (V4 a b c d) (V4 e f g h) = a * e + b * f + c * g + d * h
  {-# INLINE dot #-}

instance Distributive V4 where
  distribute f = V4 (fmap (\(V4 x _ _ _) -> x) f)
                    (fmap (\(V4 _ y _ _) -> y) f)
                    (fmap (\(V4 _ _ z _) -> z) f)
                    (fmap (\(V4 _ _ _ w) -> w) f)
  {-# INLINE distribute #-}

instance Hashable a => Hashable (V4 a) where
  hashWithSalt s (V4 a b c d) = s `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d
  {-# INLINE hashWithSalt #-}

-- | A space that distinguishes orthogonal basis vectors '_x', '_y', '_z', '_w'. (It may have more.)
class R3 t => R4 t where
  -- |
  -- @
  -- '_w' :: 'Lens'' (t a) a
  -- @
  _w :: Functor f => (a -> f a) -> t a -> f (t a)
  -- |
  -- @
  -- '_xyzw' :: 'Lens'' (t a) ('V4' a)
  -- @
  _xyzw :: Functor f => (V4 a -> f (V4 a)) -> t a -> f (t a)

-- |
-- @
-- '_xw' :: 'R4' t => 'Lens'' (t a) ('V2' a)
-- @
_xw :: (R4 t, Functor f) => (V2 a -> f (V2 a)) -> t a -> f (t a)
_xw f = _xyzw $ \(V4 a b c d) -> f (V2 a d) <&> \(V2 a' d') -> V4 a' b c d'
{-# INLINE _xw #-}

-- |
-- @
-- '_yw' :: 'R4' t => 'Lens'' (t a) ('V2' a)
-- @
_yw :: (R4 t, Functor f) => (V2 a -> f (V2 a)) -> t a -> f (t a)
_yw f = _xyzw $ \(V4 a b c d) -> f (V2 b d) <&> \(V2 b' d') -> V4 a b' c d'
{-# INLINE _yw #-}

-- |
-- @
-- '_zw' :: 'R4' t => 'Lens'' (t a) ('V2' a)
-- @
_zw :: (R4 t, Functor f) => (V2 a -> f (V2 a)) -> t a -> f (t a)
_zw f = _xyzw $ \(V4 a b c d) -> f (V2 c d) <&> \(V2 c' d') -> V4 a b c' d'
{-# INLINE _zw #-}

-- |
-- @
-- '_wx' :: 'R4' t => 'Lens'' (t a) ('V2' a)
-- @
_wx :: (R4 t, Functor f) => (V2 a -> f (V2 a)) -> t a -> f (t a)
_wx f = _xyzw $ \(V4 a b c d) -> f (V2 d a) <&> \(V2 d' a') -> V4 a' b c d'
{-# INLINE _wx #-}

-- |
-- @
-- '_wy' :: 'R4' t => 'Lens'' (t a) ('V2' a)
-- @
_wy :: (R4 t, Functor f) => (V2 a -> f (V2 a)) -> t a -> f (t a)
_wy f = _xyzw $ \(V4 a b c d) -> f (V2 d b) <&> \(V2 d' b') -> V4 a b' c d'
{-# INLINE _wy #-}

-- |
-- @
-- '_wz' :: 'R4' t => 'Lens'' (t a) ('V2' a)
-- @
_wz :: (R4 t, Functor f) => (V2 a -> f (V2 a)) -> t a -> f (t a)
_wz f = _xyzw $ \(V4 a b c d) -> f (V2 d c) <&> \(V2 d' c') -> V4 a b c' d'
{-# INLINE _wz #-}

-- |
-- @
-- '_xyw' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_xyw :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_xyw f = _xyzw $ \(V4 a b c d) -> f (V3 a b d) <&> \(V3 a' b' d') -> V4 a' b' c d'
{-# INLINE _xyw #-}

-- |
-- @
-- '_xzw' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_xzw :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_xzw f = _xyzw $ \(V4 a b c d) -> f (V3 a c d) <&> \(V3 a' c' d') -> V4 a' b c' d'
{-# INLINE _xzw #-}

-- |
-- @
-- '_xwy' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_xwy :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_xwy f = _xyzw $ \(V4 a b c d) -> f (V3 a d b) <&> \(V3 a' d' b') -> V4 a' b' c d'
{-# INLINE _xwy #-}

-- |
-- @
-- '_xwz' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_xwz :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_xwz f = _xyzw $ \(V4 a b c d) -> f (V3 a d c) <&> \(V3 a' d' c') -> V4 a' b c' d'
{-# INLINE _xwz #-}

-- |
-- @
-- '_yxw' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_yxw :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_yxw f = _xyzw $ \(V4 a b c d) -> f (V3 b a d) <&> \(V3 b' a' d') -> V4 a' b' c d'
{-# INLINE _yxw #-}

-- |
-- @
-- '_yzw' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_yzw :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_yzw f = _xyzw $ \(V4 a b c d) -> f (V3 b c d) <&> \(V3 b' c' d') -> V4 a b' c' d'
{-# INLINE _yzw #-}

-- |
-- @
-- '_ywx' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_ywx :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_ywx f = _xyzw $ \(V4 a b c d) -> f (V3 b d a) <&> \(V3 b' d' a') -> V4 a' b' c d'
{-# INLINE _ywx #-}

-- |
-- @
-- '_ywz' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_ywz :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_ywz f = _xyzw $ \(V4 a b c d) -> f (V3 b d c) <&> \(V3 b' d' c') -> V4 a b' c' d'
{-# INLINE _ywz #-}

-- |
-- @
-- '_zxw' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_zxw :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_zxw f = _xyzw $ \(V4 a b c d) -> f (V3 c a d) <&> \(V3 c' a' d') -> V4 a' b c' d'
{-# INLINE _zxw #-}

-- |
-- @
-- '_zyw' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_zyw :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_zyw f = _xyzw $ \(V4 a b c d) -> f (V3 c b d) <&> \(V3 c' b' d') -> V4 a b' c' d'
{-# INLINE _zyw #-}

-- |
-- @
-- '_zwx' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_zwx :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_zwx f = _xyzw $ \(V4 a b c d) -> f (V3 c d a) <&> \(V3 c' d' a') -> V4 a' b c' d'
{-# INLINE _zwx #-}

-- |
-- @
-- '_zwy' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_zwy :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_zwy f = _xyzw $ \(V4 a b c d) -> f (V3 c d b) <&> \(V3 c' d' b') -> V4 a b' c' d'
{-# INLINE _zwy #-}

-- |
-- @
-- '_wxy' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_wxy :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_wxy f = _xyzw $ \(V4 a b c d) -> f (V3 d a b) <&> \(V3 d' a' b') -> V4 a' b' c d'
{-# INLINE _wxy #-}

-- |
-- @
-- '_wxz' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_wxz :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_wxz f = _xyzw $ \(V4 a b c d) -> f (V3 d a c) <&> \(V3 d' a' c') -> V4 a' b c' d'
{-# INLINE _wxz #-}

-- |
-- @
-- '_wyx' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_wyx :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_wyx f = _xyzw $ \(V4 a b c d) -> f (V3 d b a) <&> \(V3 d' b' a') -> V4 a' b' c d'
{-# INLINE _wyx #-}

-- |
-- @
-- '_wyz' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_wyz :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_wyz f = _xyzw $ \(V4 a b c d) -> f (V3 d b c) <&> \(V3 d' b' c') -> V4 a b' c' d'
{-# INLINE _wyz #-}

-- |
-- @
-- '_wzx' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_wzx :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_wzx f = _xyzw $ \(V4 a b c d) -> f (V3 d c a) <&> \(V3 d' c' a') -> V4 a' b c' d'
{-# INLINE _wzx #-}

-- |
-- @
-- '_wzy' :: 'R4' t => 'Lens'' (t a) ('V3' a)
-- @
_wzy :: (R4 t, Functor f) => (V3 a -> f (V3 a)) -> t a -> f (t a)
_wzy f = _xyzw $ \(V4 a b c d) -> f (V3 d c b) <&> \(V3 d' c' b') -> V4 a b' c' d'
{-# INLINE _wzy #-}

-- |
-- @
-- '_xywz' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_xywz :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_xywz f = _xyzw $ \(V4 a b c d) -> f (V4 a b d c) <&> \(V4 a' b' d' c') -> V4 a' b' c' d'
{-# INLINE _xywz #-}

-- |
-- @
-- '_xzyw' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_xzyw :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_xzyw f = _xyzw $ \(V4 a b c d) -> f (V4 a c b d) <&> \(V4 a' c' b' d') -> V4 a' b' c' d'
{-# INLINE _xzyw #-}

-- |
-- @
-- '_xzwy' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_xzwy :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_xzwy f = _xyzw $ \(V4 a b c d) -> f (V4 a c d b) <&> \(V4 a' c' d' b') -> V4 a' b' c' d'
{-# INLINE _xzwy #-}

-- |
-- @
-- '_xwyz' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_xwyz :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_xwyz f = _xyzw $ \(V4 a b c d) -> f (V4 a d b c) <&> \(V4 a' d' b' c') -> V4 a' b' c' d'
{-# INLINE _xwyz #-}

-- |
-- @
-- '_xwzy' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_xwzy :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_xwzy f = _xyzw $ \(V4 a b c d) -> f (V4 a d c b) <&> \(V4 a' d' c' b') -> V4 a' b' c' d'
{-# INLINE _xwzy #-}

-- |
-- @
-- '_yxzw' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_yxzw :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_yxzw f = _xyzw $ \(V4 a b c d) -> f (V4 b a c d) <&> \(V4 b' a' c' d') -> V4 a' b' c' d'
{-# INLINE _yxzw #-}

-- |
-- @
-- '_yxwz' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_yxwz :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_yxwz f = _xyzw $ \(V4 a b c d) -> f (V4 b a d c) <&> \(V4 b' a' d' c') -> V4 a' b' c' d'
{-# INLINE _yxwz #-}

-- |
-- @
-- '_yzxw' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_yzxw :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_yzxw f = _xyzw $ \(V4 a b c d) -> f (V4 b c a d) <&> \(V4 b' c' a' d') -> V4 a' b' c' d'
{-# INLINE _yzxw #-}

-- |
-- @
-- '_yzwx' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_yzwx :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_yzwx f = _xyzw $ \(V4 a b c d) -> f (V4 b c d a) <&> \(V4 b' c' d' a') -> V4 a' b' c' d'
{-# INLINE _yzwx #-}

-- |
-- @
-- '_ywxz' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_ywxz :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_ywxz f = _xyzw $ \(V4 a b c d) -> f (V4 b d a c) <&> \(V4 b' d' a' c') -> V4 a' b' c' d'
{-# INLINE _ywxz #-}

-- |
-- @
-- '_ywzx' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_ywzx :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_ywzx f = _xyzw $ \(V4 a b c d) -> f (V4 b d c a) <&> \(V4 b' d' c' a') -> V4 a' b' c' d'
{-# INLINE _ywzx #-}

-- |
-- @
-- '_zxyw' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_zxyw :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_zxyw f = _xyzw $ \(V4 a b c d) -> f (V4 c a b d) <&> \(V4 c' a' b' d') -> V4 a' b' c' d'
{-# INLINE _zxyw #-}

-- |
-- @
-- '_zxwy' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_zxwy :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_zxwy f = _xyzw $ \(V4 a b c d) -> f (V4 c a d b) <&> \(V4 c' a' d' b') -> V4 a' b' c' d'
{-# INLINE _zxwy #-}

-- |
-- @
-- '_zyxw' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_zyxw :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_zyxw f = _xyzw $ \(V4 a b c d) -> f (V4 c b a d) <&> \(V4 c' b' a' d') -> V4 a' b' c' d'
{-# INLINE _zyxw #-}

-- |
-- @
-- '_zywx' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_zywx :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_zywx f = _xyzw $ \(V4 a b c d) -> f (V4 c b d a) <&> \(V4 c' b' d' a') -> V4 a' b' c' d'
{-# INLINE _zywx #-}

-- |
-- @
-- '_zwxy' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_zwxy :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_zwxy f = _xyzw $ \(V4 a b c d) -> f (V4 c d a b) <&> \(V4 c' d' a' b') -> V4 a' b' c' d'
{-# INLINE _zwxy #-}

-- |
-- @
-- '_zwyx' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_zwyx :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_zwyx f = _xyzw $ \(V4 a b c d) -> f (V4 c d b a) <&> \(V4 c' d' b' a') -> V4 a' b' c' d'
{-# INLINE _zwyx #-}

-- |
-- @
-- '_wxyz' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_wxyz :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_wxyz f = _xyzw $ \(V4 a b c d) -> f (V4 d a b c) <&> \(V4 d' a' b' c') -> V4 a' b' c' d'
{-# INLINE _wxyz #-}

-- |
-- @
-- '_wxzy' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_wxzy :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_wxzy f = _xyzw $ \(V4 a b c d) -> f (V4 d a c b) <&> \(V4 d' a' c' b') -> V4 a' b' c' d'
{-# INLINE _wxzy #-}

-- |
-- @
-- '_wyxz' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_wyxz :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_wyxz f = _xyzw $ \(V4 a b c d) -> f (V4 d b a c) <&> \(V4 d' b' a' c') -> V4 a' b' c' d'
{-# INLINE _wyxz #-}

-- |
-- @
-- '_wyzx' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_wyzx :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_wyzx f = _xyzw $ \(V4 a b c d) -> f (V4 d b c a) <&> \(V4 d' b' c' a') -> V4 a' b' c' d'
{-# INLINE _wyzx #-}

-- |
-- @
-- '_wzxy' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_wzxy :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_wzxy f = _xyzw $ \(V4 a b c d) -> f (V4 d c a b) <&> \(V4 d' c' a' b') -> V4 a' b' c' d'
{-# INLINE _wzxy #-}

-- |
-- @
-- '_wzyx' :: 'R4' t => 'Lens'' (t a) ('V4' a)
-- @
_wzyx :: (R4 t, Functor f) => (V4 a -> f (V4 a)) -> t a -> f (t a)
_wzyx f = _xyzw $ \(V4 a b c d) -> f (V4 d c b a) <&> \(V4 d' c' b' a') -> V4 a' b' c' d'
{-# INLINE _wzyx #-}

ew :: R4 t => E t
ew = E _w

instance R1 V4 where
  _x f (V4 a b c d) = (\a' -> V4 a' b c d) <$> f a
  {-# INLINE _x #-}

instance R2 V4 where
  _y f (V4 a b c d) = (\b' -> V4 a b' c d) <$> f b
  {-# INLINE _y #-}
  _xy f (V4 a b c d) = (\(V2 a' b') -> V4 a' b' c d) <$> f (V2 a b)
  {-# INLINE _xy #-}

instance R3 V4 where
  _z f (V4 a b c d) = (\c' -> V4 a b c' d) <$> f c
  {-# INLINE _z #-}
  _xyz f (V4 a b c d) = (\(V3 a' b' c') -> V4 a' b' c' d) <$> f (V3 a b c)
  {-# INLINE _xyz #-}

instance R4 V4 where
  _w f (V4 a b c d) = V4 a b c <$> f d
  {-# INLINE _w #-}
  _xyzw = id
  {-# INLINE _xyzw #-}

instance Storable a => Storable (V4 a) where
  sizeOf _ = 4 * sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (V4 x y z w) = do poke ptr' x
                             pokeElemOff ptr' 1 y
                             pokeElemOff ptr' 2 z
                             pokeElemOff ptr' 3 w
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = V4 <$> peek ptr' <*> peekElemOff ptr' 1
                <*> peekElemOff ptr' 2 <*> peekElemOff ptr' 3
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

-- | Convert a 3-dimensional affine vector into a 4-dimensional homogeneous vector.
vector :: Num a => V3 a -> V4 a
vector (V3 a b c) = V4 a b c 0
{-# INLINE vector #-}

-- | Convert a 3-dimensional affine point into a 4-dimensional homogeneous vector.
point :: Num a => V3 a -> V4 a
point (V3 a b c) = V4 a b c 1
{-# INLINE point #-}

-- | Convert 4-dimensional projective coordinates to a 3-dimensional
-- point. This operation may be denoted, @euclidean [x:y:z:w] = (x\/w,
-- y\/w, z\/w)@ where the projective, homogenous, coordinate
-- @[x:y:z:w]@ is one of many associated with a single point @(x\/w,
-- y\/w, z\/w)@.
normalizePoint :: Fractional a => V4 a -> V3 a
normalizePoint (V4 a b c w) = (1/w) *^ V3 a b c
{-# INLINE normalizePoint #-}

instance Epsilon a => Epsilon (V4 a) where
  nearZero = nearZero . quadrance
  {-# INLINE nearZero #-}

instance Ix a => Ix (V4 a) where
  {-# SPECIALISE instance Ix (V4 Int) #-}

  range (V4 l1 l2 l3 l4,V4 u1 u2 u3 u4) =
    [V4 i1 i2 i3 i4 | i1 <- range (l1,u1)
                    , i2 <- range (l2,u2)
                    , i3 <- range (l3,u3)
                    , i4 <- range (l4,u4)
                    ]
  {-# INLINE range #-}

  unsafeIndex (V4 l1 l2 l3 l4,V4 u1 u2 u3 u4) (V4 i1 i2 i3 i4) =
    unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
    unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
    unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) *
    unsafeIndex (l1,u1) i1))
  {-# INLINE unsafeIndex #-}

  inRange (V4 l1 l2 l3 l4,V4 u1 u2 u3 u4) (V4 i1 i2 i3 i4) =
    inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
    inRange (l3,u3) i3 && inRange (l4,u4) i4
  {-# INLINE inRange #-}

instance Representable V4 where
  type Rep V4 = E V4
  tabulate f = V4 (f ex) (f ey) (f ez) (f ew)
  {-# INLINE tabulate #-}
  index xs (E l) = view l xs
  {-# INLINE index #-}

instance FunctorWithIndex (E V4) V4 where
  imap f (V4 a b c d) = V4 (f ex a) (f ey b) (f ez c) (f ew d)
  {-# INLINE imap #-}

instance FoldableWithIndex (E V4) V4 where
  ifoldMap f (V4 a b c d) = f ex a `mappend` f ey b `mappend` f ez c `mappend` f ew d
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex (E V4) V4 where
  itraverse f (V4 a b c d) = V4 <$> f ex a <*> f ey b <*> f ez c <*> f ew d
  {-# INLINE itraverse #-}

type instance Index (V4 a) = E V4
type instance IxValue (V4 a) = a

instance Ixed (V4 a) where
  ix = el

instance Each (V4 a) (V4 b) a b where
  each = traverse

data instance U.Vector    (V4 a) =  V_V4 !Int (U.Vector    a)
data instance U.MVector s (V4 a) = MV_V4 !Int (U.MVector s a)
instance U.Unbox a => U.Unbox (V4 a)

instance U.Unbox a => M.MVector U.MVector (V4 a) where
  basicLength (MV_V4 n _) = n
  basicUnsafeSlice m n (MV_V4 _ v) = MV_V4 n (M.basicUnsafeSlice (4*m) (4*n) v)
  basicOverlaps (MV_V4 _ v) (MV_V4 _ u) = M.basicOverlaps v u
  basicUnsafeNew n = liftM (MV_V4 n) (M.basicUnsafeNew (4*n))
  basicUnsafeRead (MV_V4 _ v) i =
    do let o = 4*i
       x <- M.basicUnsafeRead v o
       y <- M.basicUnsafeRead v (o+1)
       z <- M.basicUnsafeRead v (o+2)
       w <- M.basicUnsafeRead v (o+3)
       return (V4 x y z w)
  basicUnsafeWrite (MV_V4 _ v) i (V4 x y z w) =
    do let o = 4*i
       M.basicUnsafeWrite v o     x
       M.basicUnsafeWrite v (o+1) y
       M.basicUnsafeWrite v (o+2) z
       M.basicUnsafeWrite v (o+3) w

instance U.Unbox a => G.Vector U.Vector (V4 a) where
  basicUnsafeFreeze (MV_V4 n v) = liftM ( V_V4 n) (G.basicUnsafeFreeze v)
  basicUnsafeThaw   ( V_V4 n v) = liftM (MV_V4 n) (G.basicUnsafeThaw   v)
  basicLength       ( V_V4 n _) = n
  basicUnsafeSlice m n (V_V4 _ v) = V_V4 n (G.basicUnsafeSlice (4*m) (4*n) v)
  basicUnsafeIndexM (V_V4 _ v) i =
    do let o = 4*i
       x <- G.basicUnsafeIndexM v o
       y <- G.basicUnsafeIndexM v (o+1)
       z <- G.basicUnsafeIndexM v (o+2)
       w <- G.basicUnsafeIndexM v (o+3)
       return (V4 x y z w)

instance MonadZip V4 where
  mzipWith = liftA2

instance MonadFix V4 where
  mfix f = V4 (let V4 a _ _ _ = f a in a)
              (let V4 _ a _ _ = f a in a)
              (let V4 _ _ a _ = f a in a)
              (let V4 _ _ _ a = f a in a)

instance Bounded a => Bounded (V4 a) where
  minBound = pure minBound
  {-# INLINE minBound #-}
  maxBound = pure maxBound
  {-# INLINE maxBound #-}

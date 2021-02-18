{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
#endif
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DataKinds #-}
#endif

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE DeriveLift #-}
#endif

#ifndef MIN_VERSION_hashable
#define MIN_VERSION_hashable(x,y,z) 1
#endif

#ifndef MIN_VERSION_vector
#define MIN_VERSION_vector(x,y,z) 1
#endif

#ifndef MIN_VERSION_transformers
#define MIN_VERSION_transformers(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-2015 Edward Kmett
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
import Control.DeepSeq (NFData(rnf))
import Control.Monad (liftM)
import Control.Monad.Fix
import Control.Monad.Zip
import Control.Lens as Lens hiding ((<.>))
import Data.Binary as Binary
import Data.Bytes.Serial
import Data.Data
import Data.Distributive
import Data.Foldable
import qualified Data.Foldable.WithIndex as WithIndex
import Data.Functor.Bind
import Data.Functor.Classes
import Data.Functor.Rep
import qualified Data.Functor.WithIndex as WithIndex
import Data.Hashable
#if (MIN_VERSION_hashable(1,2,5))
import Data.Hashable.Lifted
#endif
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Semigroup.Foldable
import Data.Serialize as Cereal
import qualified Data.Traversable.WithIndex as WithIndex
#if __GLASGOW_HASKELL__ >= 707
import qualified Data.Vector as V
#endif
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import GHC.Arr (Ix(..))
#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif
#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
#endif
#if __GLASGOW_HASKELL__ >= 800
import Language.Haskell.TH.Syntax (Lift)
#endif
import Linear.Epsilon
import Linear.Metric
#if __GLASGOW_HASKELL__ >= 707
import Linear.V
#endif
import Linear.V2
import Linear.V3
import Linear.Vector
import System.Random

-- $setup
-- >>> import Control.Lens hiding (index)

-- | A 4-dimensional vector.
data V4 a = V4 !a !a !a !a deriving (Eq,Ord,Show,Read,Data,Typeable
#if __GLASGOW_HASKELL__ >= 702
                                    ,Generic
#endif
#if __GLASGOW_HASKELL__ >= 706
                                    ,Generic1
#endif
#if __GLASGOW_HASKELL__ >= 800
                                    ,Lift
#endif
                                    )

#if __GLASGOW_HASKELL__ >= 707
instance Finite V4 where
  type Size V4 = 4
  toV (V4 a b c d) = V (V.fromListN 4 [a,b,c,d])
  fromV (V v) = V4 (v V.! 0) (v V.! 1) (v V.! 2) (v V.! 3)
#endif

instance Functor V4 where
  fmap f (V4 a b c d) = V4 (f a) (f b) (f c) (f d)
  {-# INLINE fmap #-}
  a <$ _ = V4 a a a a
  {-# INLINE (<$) #-}

instance Foldable V4 where
  foldMap f (V4 a b c d) = f a `mappend` f b `mappend` f c `mappend` f d
  {-# INLINE foldMap #-}
#if __GLASGOW_HASKELL__ >= 710
  null _ = False
  length _ = 4
#endif

instance Random a => Random (V4 a) where
  random g = case random g of
    (a, g') -> case random g' of
      (b, g'') -> case random g'' of
        (c, g''') -> case random g''' of
          (d, g'''') -> (V4 a b c d, g'''')
  randomR (V4 a b c d, V4 a' b' c' d') g = case randomR (a,a') g of
    (a'', g') -> case randomR (b,b') g' of
      (b'', g'') -> case randomR (c,c') g'' of
        (c'', g''') -> case randomR (d,d') g''' of
          (d'', g'''') -> (V4 a'' b'' c'' d'', g'''')

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

instance Floating a => Floating (V4 a) where
    pi = pure pi
    {-# INLINE pi #-}
    exp = fmap exp
    {-# INLINE exp #-}
    sqrt = fmap sqrt
    {-# INLINE sqrt #-}
    log = fmap log
    {-# INLINE log #-}
    (**) = liftA2 (**)
    {-# INLINE (**) #-}
    logBase = liftA2 logBase
    {-# INLINE logBase #-}
    sin = fmap sin
    {-# INLINE sin #-}
    tan = fmap tan
    {-# INLINE tan #-}
    cos = fmap cos
    {-# INLINE cos #-}
    asin = fmap asin
    {-# INLINE asin #-}
    atan = fmap atan
    {-# INLINE atan #-}
    acos = fmap acos
    {-# INLINE acos #-}
    sinh = fmap sinh
    {-# INLINE sinh #-}
    tanh = fmap tanh
    {-# INLINE tanh #-}
    cosh = fmap cosh
    {-# INLINE cosh #-}
    asinh = fmap asinh
    {-# INLINE asinh #-}
    atanh = fmap atanh
    {-# INLINE atanh #-}
    acosh = fmap acosh
    {-# INLINE acosh #-}

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

#if (MIN_VERSION_hashable(1,2,5))
instance Hashable1 V4 where
  liftHashWithSalt h s (V4 a b c d) = s `h` a `h` b `h` c `h` d
  {-# INLINE liftHashWithSalt #-}
#endif

-- | A space that distinguishes orthogonal basis vectors '_x', '_y', '_z', '_w'. (It may have more.)
class R3 t => R4 t where
  -- |
  -- >>> V4 1 2 3 4 ^._w
  -- 4
  _w :: Lens' (t a) a
  _xyzw :: Lens' (t a) (V4 a)

_xw, _yw, _zw, _wx, _wy, _wz :: R4 t => Lens' (t a) (V2 a)
_xw f = _xyzw $ \(V4 a b c d) -> f (V2 a d) <&> \(V2 a' d') -> V4 a' b c d'
{-# INLINE _xw #-}

_yw f = _xyzw $ \(V4 a b c d) -> f (V2 b d) <&> \(V2 b' d') -> V4 a b' c d'
{-# INLINE _yw #-}

_zw f = _xyzw $ \(V4 a b c d) -> f (V2 c d) <&> \(V2 c' d') -> V4 a b c' d'
{-# INLINE _zw #-}

_wx f = _xyzw $ \(V4 a b c d) -> f (V2 d a) <&> \(V2 d' a') -> V4 a' b c d'
{-# INLINE _wx #-}

_wy f = _xyzw $ \(V4 a b c d) -> f (V2 d b) <&> \(V2 d' b') -> V4 a b' c d'
{-# INLINE _wy #-}

_wz f = _xyzw $ \(V4 a b c d) -> f (V2 d c) <&> \(V2 d' c') -> V4 a b c' d'
{-# INLINE _wz #-}

_xyw, _xzw, _xwy, _xwz, _yxw, _yzw, _ywx, _ywz, _zxw, _zyw, _zwx, _zwy, _wxy, _wxz, _wyx, _wyz, _wzx, _wzy :: R4 t => Lens' (t a) (V3 a)
_xyw f = _xyzw $ \(V4 a b c d) -> f (V3 a b d) <&> \(V3 a' b' d') -> V4 a' b' c d'
{-# INLINE _xyw #-}

_xzw f = _xyzw $ \(V4 a b c d) -> f (V3 a c d) <&> \(V3 a' c' d') -> V4 a' b c' d'
{-# INLINE _xzw #-}

_xwy f = _xyzw $ \(V4 a b c d) -> f (V3 a d b) <&> \(V3 a' d' b') -> V4 a' b' c d'
{-# INLINE _xwy #-}

_xwz f = _xyzw $ \(V4 a b c d) -> f (V3 a d c) <&> \(V3 a' d' c') -> V4 a' b c' d'
{-# INLINE _xwz #-}

_yxw f = _xyzw $ \(V4 a b c d) -> f (V3 b a d) <&> \(V3 b' a' d') -> V4 a' b' c d'
{-# INLINE _yxw #-}

_yzw f = _xyzw $ \(V4 a b c d) -> f (V3 b c d) <&> \(V3 b' c' d') -> V4 a b' c' d'
{-# INLINE _yzw #-}

_ywx f = _xyzw $ \(V4 a b c d) -> f (V3 b d a) <&> \(V3 b' d' a') -> V4 a' b' c d'
{-# INLINE _ywx #-}

_ywz f = _xyzw $ \(V4 a b c d) -> f (V3 b d c) <&> \(V3 b' d' c') -> V4 a b' c' d'
{-# INLINE _ywz #-}

_zxw f = _xyzw $ \(V4 a b c d) -> f (V3 c a d) <&> \(V3 c' a' d') -> V4 a' b c' d'
{-# INLINE _zxw #-}

_zyw f = _xyzw $ \(V4 a b c d) -> f (V3 c b d) <&> \(V3 c' b' d') -> V4 a b' c' d'
{-# INLINE _zyw #-}

_zwx f = _xyzw $ \(V4 a b c d) -> f (V3 c d a) <&> \(V3 c' d' a') -> V4 a' b c' d'
{-# INLINE _zwx #-}

_zwy f = _xyzw $ \(V4 a b c d) -> f (V3 c d b) <&> \(V3 c' d' b') -> V4 a b' c' d'
{-# INLINE _zwy #-}

_wxy f = _xyzw $ \(V4 a b c d) -> f (V3 d a b) <&> \(V3 d' a' b') -> V4 a' b' c d'
{-# INLINE _wxy #-}

_wxz f = _xyzw $ \(V4 a b c d) -> f (V3 d a c) <&> \(V3 d' a' c') -> V4 a' b c' d'
{-# INLINE _wxz #-}

_wyx f = _xyzw $ \(V4 a b c d) -> f (V3 d b a) <&> \(V3 d' b' a') -> V4 a' b' c d'
{-# INLINE _wyx #-}

_wyz f = _xyzw $ \(V4 a b c d) -> f (V3 d b c) <&> \(V3 d' b' c') -> V4 a b' c' d'
{-# INLINE _wyz #-}

_wzx f = _xyzw $ \(V4 a b c d) -> f (V3 d c a) <&> \(V3 d' c' a') -> V4 a' b c' d'
{-# INLINE _wzx #-}

_wzy f = _xyzw $ \(V4 a b c d) -> f (V3 d c b) <&> \(V3 d' c' b') -> V4 a b' c' d'
{-# INLINE _wzy #-}

_xywz, _xzyw, _xzwy, _xwyz, _xwzy, _yxzw , _yxwz, _yzxw, _yzwx, _ywxz
  , _ywzx, _zxyw, _zxwy, _zyxw, _zywx, _zwxy, _zwyx, _wxyz, _wxzy, _wyxz
  , _wyzx, _wzxy, _wzyx :: R4 t => Lens' (t a) (V4 a)
_xywz f = _xyzw $ \(V4 a b c d) -> f (V4 a b d c) <&> \(V4 a' b' d' c') -> V4 a' b' c' d'
{-# INLINE _xywz #-}

_xzyw f = _xyzw $ \(V4 a b c d) -> f (V4 a c b d) <&> \(V4 a' c' b' d') -> V4 a' b' c' d'
{-# INLINE _xzyw #-}

_xzwy f = _xyzw $ \(V4 a b c d) -> f (V4 a c d b) <&> \(V4 a' c' d' b') -> V4 a' b' c' d'
{-# INLINE _xzwy #-}

_xwyz f = _xyzw $ \(V4 a b c d) -> f (V4 a d b c) <&> \(V4 a' d' b' c') -> V4 a' b' c' d'
{-# INLINE _xwyz #-}

_xwzy f = _xyzw $ \(V4 a b c d) -> f (V4 a d c b) <&> \(V4 a' d' c' b') -> V4 a' b' c' d'
{-# INLINE _xwzy #-}

_yxzw f = _xyzw $ \(V4 a b c d) -> f (V4 b a c d) <&> \(V4 b' a' c' d') -> V4 a' b' c' d'
{-# INLINE _yxzw #-}

_yxwz f = _xyzw $ \(V4 a b c d) -> f (V4 b a d c) <&> \(V4 b' a' d' c') -> V4 a' b' c' d'
{-# INLINE _yxwz #-}

_yzxw f = _xyzw $ \(V4 a b c d) -> f (V4 b c a d) <&> \(V4 b' c' a' d') -> V4 a' b' c' d'
{-# INLINE _yzxw #-}

_yzwx f = _xyzw $ \(V4 a b c d) -> f (V4 b c d a) <&> \(V4 b' c' d' a') -> V4 a' b' c' d'
{-# INLINE _yzwx #-}

_ywxz f = _xyzw $ \(V4 a b c d) -> f (V4 b d a c) <&> \(V4 b' d' a' c') -> V4 a' b' c' d'
{-# INLINE _ywxz #-}

_ywzx f = _xyzw $ \(V4 a b c d) -> f (V4 b d c a) <&> \(V4 b' d' c' a') -> V4 a' b' c' d'
{-# INLINE _ywzx #-}

_zxyw f = _xyzw $ \(V4 a b c d) -> f (V4 c a b d) <&> \(V4 c' a' b' d') -> V4 a' b' c' d'
{-# INLINE _zxyw #-}

_zxwy f = _xyzw $ \(V4 a b c d) -> f (V4 c a d b) <&> \(V4 c' a' d' b') -> V4 a' b' c' d'
{-# INLINE _zxwy #-}

_zyxw f = _xyzw $ \(V4 a b c d) -> f (V4 c b a d) <&> \(V4 c' b' a' d') -> V4 a' b' c' d'
{-# INLINE _zyxw #-}

_zywx f = _xyzw $ \(V4 a b c d) -> f (V4 c b d a) <&> \(V4 c' b' d' a') -> V4 a' b' c' d'
{-# INLINE _zywx #-}

_zwxy f = _xyzw $ \(V4 a b c d) -> f (V4 c d a b) <&> \(V4 c' d' a' b') -> V4 a' b' c' d'
{-# INLINE _zwxy #-}

_zwyx f = _xyzw $ \(V4 a b c d) -> f (V4 c d b a) <&> \(V4 c' d' b' a') -> V4 a' b' c' d'
{-# INLINE _zwyx #-}

_wxyz f = _xyzw $ \(V4 a b c d) -> f (V4 d a b c) <&> \(V4 d' a' b' c') -> V4 a' b' c' d'
{-# INLINE _wxyz #-}

_wxzy f = _xyzw $ \(V4 a b c d) -> f (V4 d a c b) <&> \(V4 d' a' c' b') -> V4 a' b' c' d'
{-# INLINE _wxzy #-}

_wyxz f = _xyzw $ \(V4 a b c d) -> f (V4 d b a c) <&> \(V4 d' b' a' c') -> V4 a' b' c' d'
{-# INLINE _wyxz #-}

_wyzx f = _xyzw $ \(V4 a b c d) -> f (V4 d b c a) <&> \(V4 d' b' c' a') -> V4 a' b' c' d'
{-# INLINE _wyzx #-}

_wzxy f = _xyzw $ \(V4 a b c d) -> f (V4 d c a b) <&> \(V4 d' c' a' b') -> V4 a' b' c' d'
{-# INLINE _wzxy #-}

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

-- | Convert a 3-dimensional affine vector into a 4-dimensional homogeneous vector,
-- i.e. sets the @w@ coordinate to 0.
vector :: Num a => V3 a -> V4 a
vector (V3 a b c) = V4 a b c 0
{-# INLINE vector #-}

-- | Convert a 3-dimensional affine point into a 4-dimensional homogeneous vector,
-- i.e. sets the @w@ coordinate to 1.
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

instance WithIndex.FunctorWithIndex (E V4) V4 where
  imap f (V4 a b c d) = V4 (f ex a) (f ey b) (f ez c) (f ew d)
  {-# INLINE imap #-}

instance WithIndex.FoldableWithIndex (E V4) V4 where
  ifoldMap f (V4 a b c d) = f ex a `mappend` f ey b `mappend` f ez c `mappend` f ew d
  {-# INLINE ifoldMap #-}

instance WithIndex.TraversableWithIndex (E V4) V4 where
  itraverse f (V4 a b c d) = V4 <$> f ex a <*> f ey b <*> f ez c <*> f ew d
  {-# INLINE itraverse #-}

#if !MIN_VERSION_lens(5,0,0)
instance Lens.FunctorWithIndex     (E V4) V4 where imap      = WithIndex.imap
instance Lens.FoldableWithIndex    (E V4) V4 where ifoldMap  = WithIndex.ifoldMap
instance Lens.TraversableWithIndex (E V4) V4 where itraverse = WithIndex.itraverse
#endif

type instance Index (V4 a) = E V4
type instance IxValue (V4 a) = a

instance Ixed (V4 a) where
  ix i = el i

instance Each (V4 a) (V4 b) a b where
  each = traverse

data instance U.Vector    (V4 a) =  V_V4 {-# UNPACK #-} !Int !(U.Vector    a)
data instance U.MVector s (V4 a) = MV_V4 {-# UNPACK #-} !Int !(U.MVector s a)
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
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_V4 _ v) = M.basicInitialize v
#endif

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

instance NFData a => NFData (V4 a) where
  rnf (V4 a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

instance Serial1 V4 where
  serializeWith = traverse_
  deserializeWith k = V4 <$> k <*> k <*> k <*> k

instance Serial a => Serial (V4 a) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance Binary a => Binary (V4 a) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance Serialize a => Serialize (V4 a) where
  put = serializeWith Cereal.put
  get = deserializeWith Cereal.get

#if (MIN_VERSION_transformers(0,5,0)) || !(MIN_VERSION_transformers(0,4,0))
instance Eq1 V4 where
  liftEq k (V4 a b c d) (V4 e f g h) = k a e && k b f && k c g && k d h
instance Ord1 V4 where
  liftCompare k (V4 a b c d) (V4 e f g h) = k a e `mappend` k b f `mappend` k c g `mappend` k d h
instance Read1 V4 where
  liftReadsPrec k _ z = readParen (z > 10) $ \r ->
     [ (V4 a b c d, r5)
     | ("V4",r1) <- lex r
     , (a,r2) <- k 11 r1
     , (b,r3) <- k 11 r2
     , (c,r4) <- k 11 r3
     , (d,r5) <- k 11 r4
     ]
instance Show1 V4 where
  liftShowsPrec f _ z (V4 a b c d) = showParen (z > 10) $
     showString "V4 " . f 11 a . showChar ' ' . f 11 b . showChar ' ' . f 11 c . showChar ' ' . f 11 d
#else
instance Eq1 V4 where eq1 = (==)
instance Ord1 V4 where compare1 = compare
instance Show1 V4 where showsPrec1 = showsPrec
instance Read1 V4 where readsPrec1 = readsPrec
#endif

instance Field1 (V4 a) (V4 a) a a where
  _1 f (V4 x y z w) = f x <&> \x' -> V4 x' y z w

instance Field2 (V4 a) (V4 a) a a where
  _2 f (V4 x y z w) = f y <&> \y' -> V4 x y' z w

instance Field3 (V4 a) (V4 a) a a where
  _3 f (V4 x y z w) = f z <&> \z' -> V4 x y z' w

instance Field4 (V4 a) (V4 a) a a where
  _4 f (V4 x y z w) = f w <&> \w' -> V4 x y z w'

instance Semigroup a => Semigroup (V4 a) where
 (<>) = liftA2 (<>)

instance Monoid a => Monoid (V4 a) where
  mempty = pure mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = liftA2 mappend
#endif


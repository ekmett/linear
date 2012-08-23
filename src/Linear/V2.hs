{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Linear.V2
  ( V2(..)
  , R2(..)
  , perp
  ) where

import Control.Applicative
import Control.Lens
import Data.Data
import Data.Distributive
import Data.Foldable
import Data.Monoid
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import Linear.Metric
import Linear.Epsilon

data V2 a = V2 a a deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Functor V2 where
  fmap f (V2 a b) = V2 (f a) (f b)

instance Foldable V2 where
  foldMap f (V2 a b) = f a `mappend` f b

instance Traversable V2 where
  traverse f (V2 a b) = V2 <$> f a <*> f b

instance Applicative V2 where
  pure a = V2 a a
  V2 a b <*> V2 d e = V2 (a d) (b e)

instance Monad V2 where
  return a = V2 a a
  (>>=) = bindRep

instance Num a => Num (V2 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (V2 a) where
  recip = fmap recip
  (/) = liftA2 (/)
  fromRational = pure . fromRational

instance Metric V2 where
  dot (V2 a b) (V2 c d) = a * c + b * d

class R2 t where
  _x :: Functor f => (a -> f a) -> t a -> f (t a)
  _x = _xy._x

  _y :: Functor f => (a -> f a) -> t a -> f (t a)
  _y = _xy._y

  _xy :: Functor f => (V2 a -> f (V2 a)) -> t a -> f (t a)

instance R2 V2 where
  _x f (V2 a b) = (`V2` b) <$> f a
  _y f (V2 a b) = (V2 a) <$> f b
  _xy = id

instance Representable V2 where
  rep f = V2 (f _x) (f _y)

instance Distributive V2 where
  distribute f = V2 (fmap (^._x) f) (fmap (^._y) f)

-- the counter-clockwise perpendicular vector
perp :: Num a => V2 a -> V2 a
perp (V2 a b) = V2 (negate b) a

instance Epsilon a => Epsilon (V2 a) where
  nearZero = nearZero . quadrance

instance forall a. Storable a => Storable (V2 a) where
  sizeOf _ = 2 * sizeOf (undefined::a)
  alignment _ = alignment (undefined::a)
  poke ptr (V2 x y) = poke ptr' x >> pokeElemOff ptr' sz y
    where ptr' = castPtr ptr
          sz = sizeOf (undefined::a)
  peek ptr = V2 <$> peek ptr' <*> peekElemOff ptr' sz
    where ptr' = castPtr ptr
          sz = sizeOf (undefined::a)

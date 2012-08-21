{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Linear.V2
  ( V2(..)
  , R2(..)
  , perp
  ) where

import Control.Lens
import Data.Data
import Data.Distributive
import Data.Foldable
import Data.Monoid
import Control.Applicative
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
  x :: Functor f => (a -> f a) -> t a -> f (t a)
  x = xy . x

  y :: Functor f => (a -> f a) -> t a -> f (t a)
  y = xy . y

  xy :: Functor f => (V2 a -> f (V2 a)) -> t a -> f (t a)

instance R2 V2 where
  x f (V2 a b) = (`V2` b) <$> f a
  y f (V2 a b) = (V2 a) <$> f b
  xy = id

instance Representable V2 where
  rep f = V2 (f x) (f y)

instance Distributive V2 where
  distribute f = V2 (fmap (^.x) f) (fmap (^.y) f)

-- the counter-clockwise perpendicular vector
perp :: Num a => V2 a -> V2 a
perp (V2 a b) = V2 (negate b) a

instance Epsilon a => Epsilon (V2 a) where
  nearZero = nearZero . quadrance

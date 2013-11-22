{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
#endif
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Operations on affine spaces.
-----------------------------------------------------------------------------
module Linear.Affine where

import Control.Applicative
import Data.Complex (Complex)
import Data.Foldable as Foldable
import Data.Functor.Bind
import Data.Functor.Identity (Identity)
import Data.HashMap.Lazy (HashMap)
import Data.Hashable
import Data.IntMap (IntMap)
import Data.Ix
import Data.Map (Map)
import Data.Traversable as Traversable
import Data.Vector (Vector)
import Foreign.Storable
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
#endif
import Linear.Core
import Linear.Epsilon
import Linear.Metric
import Linear.Plucker
import Linear.Quaternion
import Linear.V
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector

-- | An affine space is roughly a vector space in which we have
-- forgotten or at least pretend to have forgotten the origin.
--
-- > a .+^ (b .-. a)  =  b@
-- > (a .+^ u) .+^ v  =  a .+^ (u ^+^ v)@
-- > (a .-. b) ^+^ v  =  (a .+^ v) .-. q@
class Additive (Diff p) => Affine p where
  type Diff p :: * -> *

  infixl 6 .-.
  -- | Get the difference between two points as a vector offset.
  (.-.) :: Num a => p a -> p a -> Diff p a

  infixl 6 .+^
  -- | Add a vector offset to a point.
  (.+^) :: Num a => p a -> Diff p a -> p a

  infixl 6 .-^
  -- | Subtract a vector offset from a point.
  (.-^) :: Num a => p a -> Diff p a -> p a
  p .-^ v = p .+^ negated v
  {-# INLINE (.-^) #-}

-- | Compute the quadrance of the difference (the square of the distance)
qdA :: (Affine p, Foldable (Diff p), Num a) => p a -> p a -> a
qdA a b = Foldable.sum (fmap (join (*)) (a .-. b))

-- | Distance between two points in an affine space
distanceA :: (Floating a, Foldable (Diff p), Affine p) => p a -> p a -> a
distanceA a b = sqrt (qdA a b)

#define ADDITIVEC(CTX,T) instance CTX => Affine T where type Diff T = T ; \
  (.-.) = (^-^) ; {-# INLINE (.-.) #-} ; (.+^) = (^+^) ; {-# INLINE (.+^) #-} ; \
  (.-^) = (^-^) ; {-# INLINE (.-^) #-}
#define ADDITIVE(T) ADDITIVEC((), T)

ADDITIVE([])
ADDITIVE(Complex)
ADDITIVE(ZipList)
ADDITIVE(Maybe)
ADDITIVE(IntMap)
ADDITIVE(Identity)
ADDITIVE(Vector)
ADDITIVE(V0)
ADDITIVE(V1)
ADDITIVE(V2)
ADDITIVE(V3)
ADDITIVE(V4)
ADDITIVE(Plucker)
ADDITIVE(Quaternion)
ADDITIVE(((->) b))
ADDITIVEC(Ord k, (Map k))
ADDITIVEC((Eq k, Hashable k), (HashMap k))
ADDITIVEC(Dim n, (V n))

-- | A handy wrapper to help distinguish points from vectors at the
-- type level
newtype Point f a = P (f a)
  deriving ( Eq, Ord, Show, Read, Monad, Functor, Applicative, Foldable
           , Traversable, Apply, Bind, Additive, Metric, Core, R1, R2, R3, R4
           , Fractional , Num, Ix, Storable, Epsilon
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
           )

instance Additive f => Affine (Point f) where
  type Diff (Point f) = f
  P x .-. P y = x ^-^ y
  P x .+^ v = P (x ^+^ v)
  P x .-^ v = P (x ^-^ v)

-- | Vector spaces have origins.
origin :: (Additive f, Num a) => Point f a
origin = P zero

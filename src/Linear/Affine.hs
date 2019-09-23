{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
#endif
{-# LANGUAGE DeriveDataTypeable #-}
#if defined(__GLASGOW_HASKELL__)
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
#endif

#ifndef MIN_VERSION_hashable
#define MIN_VERSION_hashable(x,y,z) 1
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
import Control.DeepSeq
import Control.Monad (liftM)
import Control.Lens
import Data.Binary as Binary
import Data.Bytes.Serial
#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce
#endif
import Data.Complex (Complex)
import Data.Data
import Data.Distributive
import Data.Foldable as Foldable
import Data.Functor.Bind
import Data.Functor.Classes
import Data.Functor.Product
import Data.Functor.Rep as Rep
import Data.HashMap.Lazy (HashMap)
import Data.Hashable
#if (MIN_VERSION_hashable(1,2,5))
import Data.Hashable.Lifted
#endif
import Data.IntMap (IntMap)
import Data.Ix
import Data.Map (Map)
import Data.Serialize as Cereal
import Data.Vector (Vector)
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U
import Foreign.Storable
#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif
#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
#endif
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

#ifdef HLINT
{-# ANN module "HLint: ignore Unused LANGUAGE pragma" #-}
#endif

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

instance (Affine f, Affine g) => Affine (Product f g) where
  type Diff (Product f g) = Product (Diff f) (Diff g)
  Pair a b .-. Pair c d = Pair (a .-. c) (b .-. d)
  Pair a b .+^ Pair c d = Pair (a .+^ c) (b .+^ d)
  Pair a b .-^ Pair c d = Pair (a .+^ c) (b .+^ d)

-- | Compute the quadrance of the difference (the square of the distance)
qdA :: (Affine p, Foldable (Diff p), Num a) => p a -> p a -> a
qdA a b = Foldable.sum (fmap (join (*)) (a .-. b))
{-# INLINE qdA #-}

-- | Distance between two points in an affine space
distanceA :: (Floating a, Foldable (Diff p), Affine p) => p a -> p a -> a
distanceA a b = sqrt (qdA a b)
{-# INLINE distanceA #-}

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
           , Eq1, Ord1, Show1, Read1
           , Traversable, Apply, Additive, Metric
           , Fractional , Num, Ix, Storable, Epsilon
           , Hashable
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
#if __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
#if __GLASGOW_HASKELL__ >= 708
           , Typeable, Data
#endif
           )

#if __GLASGOW_HASKELL__ >= 707
instance Finite f => Finite (Point f) where
  type Size (Point f) = Size f
  toV (P v) = toV v
  fromV v = P (fromV v)
#endif

instance NFData (f a) => NFData (Point f a) where
  rnf (P x) = rnf x

instance Serial1 f => Serial1 (Point f) where
  serializeWith f (P p) = serializeWith f p
  deserializeWith m = P `liftM` deserializeWith m

instance Serial (f a) => Serial (Point f a) where
  serialize (P p) = serialize p
  deserialize = P `liftM` deserialize

instance Binary (f a) => Binary (Point f a) where
  put (P p) = Binary.put p
  get = P `liftM` Binary.get

instance Serialize (f a) => Serialize (Point f a) where
  put (P p) = Cereal.put p
  get = P `liftM` Cereal.get

#if (MIN_VERSION_hashable(1,2,5))
instance Hashable1 f => Hashable1 (Point f) where
  liftHashWithSalt h s (P f) = liftHashWithSalt h s f
  {-# INLINE liftHashWithSalt #-}
#endif

#if __GLASGOW_HASKELL__ < 708
instance forall f. Typeable1 f => Typeable1 (Point f) where
  typeOf1 _ = mkTyConApp (mkTyCon3 "linear" "Linear.Affine" "Point") [] `mkAppTy`
              typeOf1 (undefined :: f a)

deriving instance (Data (f a), Typeable1 f, Typeable a) => Data (Point f a)
#endif

lensP :: Lens' (Point g a) (g a)
lensP afb (P a) = P <$> afb a
{-# INLINE lensP #-}

_Point :: Iso' (Point f a) (f a)
_Point = iso (\(P a) -> a) P
{-# INLINE _Point #-}

instance (t ~ Point g b) => Rewrapped (Point f a) t
instance Wrapped (Point f a) where
  type Unwrapped (Point f a) = f a
  _Wrapped' = _Point
  {-# INLINE _Wrapped' #-}

#if __GLASGOW_HASKELL__ >= 708
-- These are stolen from Data.Profunctor.Unsafe
(.#) :: Coercible b a => (b -> c) -> (a -> b) -> a -> c
f .# _ = coerce f
{-# INLINE (.#) #-}

(#.) :: Coercible c b => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b
{-# INLINE (#.) #-}
#else
(.#), (#.) :: (b -> c) -> (a -> b) -> a -> c
(.#) = (.)
{-# INLINE (.#) #-}
(#.) = (.)
{-# INLINE (#.) #-}
#endif

unP :: Point f a -> f a
unP (P x) = x
{-# INLINE unP #-}

-- We can't use GND to derive 'Bind' because 'join' causes
-- role troubles. However, GHC 7.8 and above let us use
-- explicit coercions for (>>-).
instance Bind f => Bind (Point f) where
#if __GLASGOW_HASKELL__ >= 708
  (>>-) = ((P .) . (. (unP .))) #. (>>-) .# unP
#else
  P m >>- f = P $ m >>- unP . f
#endif
  join (P m) = P $ m >>- \(P m') -> m'

instance Distributive f => Distributive (Point f) where
  distribute = P . collect (\(P p) -> p)
  collect = (P .) #. collect .# (unP .)

instance Representable f => Representable (Point f) where
  type Rep (Point f) = Rep f
  tabulate = P #. tabulate
  {-# INLINE tabulate #-}
  index = Rep.index .# unP
  {-# INLINE index #-}

type instance Index (Point f a) = Index (f a)
type instance IxValue (Point f a) = IxValue (f a)

instance Ixed (f a) => Ixed (Point f a) where
  ix l = lensP . ix l
  {-# INLINE ix #-}

instance Traversable f => Each (Point f a) (Point f b) a b where
  each = traverse
  {-# INLINE each #-}

instance R1 f => R1 (Point f) where
  _x = lensP . _x
  {-# INLINE _x #-}

instance R2 f => R2 (Point f) where
  _y = lensP . _y
  {-# INLINE _y #-}
  _xy = lensP . _xy
  {-# INLINE _xy #-}

instance R3 f => R3 (Point f) where
  _z = lensP . _z
  {-# INLINE _z #-}
  _xyz = lensP . _xyz
  {-# INLINE _xyz #-}

instance R4 f => R4 (Point f) where
  _w = lensP . _w
  {-# INLINE _w #-}
  _xyzw = lensP . _xyzw
  {-# INLINE _xyzw #-}

instance Additive f => Affine (Point f) where
  type Diff (Point f) = f
  (.-.) = (. unP) #. (^-^) .# unP
  {-# INLINE (.-.) #-}
  (.+^) = (P .) #. (^+^) .# unP
  {-# INLINE (.+^) #-}
  (.-^) = (P .) #. (^-^) .# unP
  {-# INLINE (.-^) #-}

-- | Vector spaces have origins.
origin :: (Additive f, Num a) => Point f a
origin = P zero

-- | An isomorphism between points and vectors, given a reference
--   point.
relative :: (Additive f, Num a) => Point f a -> Iso' (Point f a) (f a)
relative p0 = iso (.-. p0) (p0 .+^)
{-# INLINE relative #-}

newtype instance U.Vector    (Point f a) =  V_P (U.Vector    (f a))
newtype instance U.MVector s (Point f a) = MV_P (U.MVector s (f a))
instance U.Unbox (f a) => U.Unbox (Point f a)

instance U.Unbox (f a) => M.MVector U.MVector (Point f a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  basicLength (MV_P v) = M.basicLength v
  basicUnsafeSlice m n (MV_P v) = MV_P (M.basicUnsafeSlice m n v)
  basicOverlaps (MV_P v) (MV_P u) = M.basicOverlaps v u
  basicUnsafeNew n = MV_P `liftM` M.basicUnsafeNew n
  basicUnsafeRead (MV_P v) i = P `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_P v) i (P x) = M.basicUnsafeWrite v i x
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_P v) = M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif

instance U.Unbox (f a) => G.Vector U.Vector (Point f a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze (MV_P v) = V_P `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw   ( V_P v) = MV_P `liftM` G.basicUnsafeThaw   v
  basicLength       ( V_P v) = G.basicLength v
  basicUnsafeSlice m n (V_P v) = V_P (G.basicUnsafeSlice m n v)
  basicUnsafeIndexM (V_P v) i = P `liftM` G.basicUnsafeIndexM v i

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}

#ifndef MIN_VERSION_hashable
#define MIN_VERSION_hashable(x,y,z) 1
#endif

#ifndef MIN_VERSION_vector
#define MIN_VERSION_vector(x,y,z) 1
#endif

#ifndef MIN_VERSION_transformers
#define MIN_VERSION_transformers(x,y,z) 1
#endif

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
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
-- 2-D Vectors
----------------------------------------------------------------------------
module Linear.V2
  ( V2(..)
  , R1(..)
  , R2(..)
  , _yx
  , ex, ey
  , perp
  , angle
  , unangle
  , crossZ
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
import Data.Hashable.Lifted
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Serialize as Cereal
import qualified Data.Traversable.WithIndex as WithIndex
import qualified Data.Vector as V
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import GHC.Arr (Ix(..))
import GHC.Generics (Generic, Generic1)
#if defined(MIN_VERSION_template_haskell)
import Language.Haskell.TH.Syntax (Lift)
#endif
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U
import Linear.Metric
import Linear.Epsilon
import Linear.V
import Linear.Vector
import Linear.V1 (R1(..),ex)
import Prelude hiding (sum)
import System.Random (Random(..))

-- $setup
-- >>> import Control.Applicative
-- >>> import Control.Lens
-- >>> import qualified Data.Foldable as F
-- >>> let sum xs = F.sum xs

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

data V2 a = V2 !a !a deriving
  (Eq,Ord,Show,Read,Data
  ,Generic,Generic1
#if defined(MIN_VERSION_template_haskell)
  ,Lift
#endif
  )

instance Finite V2 where
  type Size V2 = 2
  toV (V2 a b) = V (V.fromListN 2 [a,b])
  fromV (V v) = V2 (v V.! 0) (v V.! 1)

instance Random a => Random (V2 a) where
  random g = case random g of
   (a, g') -> case random g' of
     (b, g'') -> (V2 a b, g'')
  {-# inline random #-}
  randomR (V2 a b, V2 c d) g = case randomR (a, c) g of
    (x, g') -> case randomR (b, d) g' of
      (y, g'') -> (V2 x y, g'')
  {-# inline randomR #-}

instance Functor V2 where
  fmap f (V2 a b) = V2 (f a) (f b)
  {-# INLINE fmap #-}
  a <$ _ = V2 a a
  {-# INLINE (<$) #-}

instance Foldable V2 where
  foldMap f (V2 a b) = f a `mappend` f b
  {-# INLINE foldMap #-}
  null _ = False
  length _ = 2

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
  {-# INLINE (<.>) #-}

instance Applicative V2 where
  pure a = V2 a a
  {-# INLINE pure #-}
  V2 a b <*> V2 d e = V2 (a d) (b e)
  {-# INLINE (<*>) #-}

instance Hashable a => Hashable (V2 a) where
  hashWithSalt s (V2 a b) = s `hashWithSalt` a `hashWithSalt` b
  {-# INLINE hashWithSalt #-}

instance Hashable1 V2 where
  liftHashWithSalt h s (V2 a b) = s `h` a `h` b
  {-# INLINE liftHashWithSalt #-}

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
#if !(MIN_VERSION_base(4,11,0))
  return a = V2 a a
  {-# INLINE return #-}
#endif
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

instance Floating a => Floating (V2 a) where
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
  _y :: Lens' (t a) a
  _y = _xy._y
  {-# INLINE _y #-}

  _xy :: Lens' (t a) (V2 a)

-- |
-- >>> V2 1 2 ^. _yx
-- V2 2 1
_yx :: R2 t => Lens' (t a) (V2 a)
_yx f = _xy $ \(V2 a b) -> f (V2 b a) <&> \(V2 b' a') -> V2 a' b'
{-# INLINE _yx #-}

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

instance WithIndex.FunctorWithIndex (E V2) V2 where
  imap f (V2 a b) = V2 (f ex a) (f ey b)
  {-# INLINE imap #-}

instance WithIndex.FoldableWithIndex (E V2) V2 where
  ifoldMap f (V2 a b) = f ex a `mappend` f ey b
  {-# INLINE ifoldMap #-}

instance WithIndex.TraversableWithIndex (E V2) V2 where
  itraverse f (V2 a b) = V2 <$> f ex a <*> f ey b
  {-# INLINE itraverse #-}

#if !MIN_VERSION_lens(5,0,0)
instance Lens.FunctorWithIndex     (E V2) V2 where imap      = WithIndex.imap
instance Lens.FoldableWithIndex    (E V2) V2 where ifoldMap  = WithIndex.ifoldMap
instance Lens.TraversableWithIndex (E V2) V2 where itraverse = WithIndex.itraverse
#endif

type instance Index (V2 a) = E V2
type instance IxValue (V2 a) = a

instance Ixed (V2 a) where
  ix i = el i
  {-# INLINE ix #-}

instance Each (V2 a) (V2 b) a b where
  each = traverse
  {-# INLINE each #-}

data instance U.Vector    (V2 a) =  V_V2 {-# UNPACK #-} !Int !(U.Vector    a)
data instance U.MVector s (V2 a) = MV_V2 {-# UNPACK #-} !Int !(U.MVector s a)
instance U.Unbox a => U.Unbox (V2 a)

instance U.Unbox a => M.MVector U.MVector (V2 a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
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
  basicInitialize (MV_V2 _ v) = M.basicInitialize v
  {-# INLINE basicInitialize #-}

instance U.Unbox a => G.Vector U.Vector (V2 a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze (MV_V2 n v) = liftM ( V_V2 n) (G.basicUnsafeFreeze v)
  basicUnsafeThaw   ( V_V2 n v) = liftM (MV_V2 n) (G.basicUnsafeThaw   v)
  basicLength       ( V_V2 n _) = n
  basicUnsafeSlice m n (V_V2 _ v) = V_V2 n (G.basicUnsafeSlice (2*m) (2*n) v)
  basicUnsafeIndexM (V_V2 _ v) i =
    do let o = 2*i
       x <- G.basicUnsafeIndexM v o
       y <- G.basicUnsafeIndexM v (o+1)
       return (V2 x y)

instance MonadZip V2 where
  mzipWith = liftA2

instance MonadFix V2 where
  mfix f = V2 (let V2 a _ = f a in a)
              (let V2 _ a = f a in a)

angle :: Floating a => a -> V2 a
angle a = V2 (cos a) (sin a)

unangle :: (Floating a, Ord a) => V2 a -> a
unangle a@(V2 ax ay) =
  let alpha = asin $ ay / norm a
  in if ax < 0
       then pi - alpha
       else alpha

-- | The Z-component of the cross product of two vectors in the XY-plane.
--
-- >>> crossZ (V2 1 0) (V2 0 1)
-- 1
crossZ :: Num a => V2 a -> V2 a -> a
crossZ (V2 x1 y1) (V2 x2 y2) = x1*y2 - y1*x2
{-# INLINE crossZ #-}

instance Bounded a => Bounded (V2 a) where
  minBound = pure minBound
  {-# INLINE minBound #-}
  maxBound = pure maxBound
  {-# INLINE maxBound #-}

instance NFData a => NFData (V2 a) where
  rnf (V2 a b) = rnf a `seq` rnf b

instance Serial1 V2 where
  serializeWith = traverse_
  deserializeWith k = V2 <$> k <*> k

instance Serial a => Serial (V2 a) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance Binary a => Binary (V2 a) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance Serialize a => Serialize (V2 a) where
  put = serializeWith Cereal.put
  get = deserializeWith Cereal.get

instance Eq1 V2 where
  liftEq f (V2 a b) (V2 c d) = f a c && f b d
instance Ord1 V2 where
  liftCompare f (V2 a b) (V2 c d) = f a c `mappend` f b d
instance Read1 V2 where
  liftReadsPrec f _ = readsData $ readsBinaryWith f f "V2" V2
instance Show1 V2 where
  liftShowsPrec f _ d (V2 a b) = showsBinaryWith f f "V2" d a b

instance Field1 (V2 a) (V2 a) a a where
  _1 f (V2 x y) = f x <&> \x' -> V2 x' y

instance Field2 (V2 a) (V2 a) a a where
  _2 f (V2 x y) = f y <&> \y' -> V2 x y'

instance Semigroup a => Semigroup (V2 a) where
 (<>) = liftA2 (<>)

instance Monoid a => Monoid (V2 a) where
  mempty = pure mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = liftA2 mappend
#endif

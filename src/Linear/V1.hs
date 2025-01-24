{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
-- 1-D Vectors
----------------------------------------------------------------------------
module Linear.V1
  ( V1(..)
  , R1(..)
  , ex
  ) where

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Monad (liftM)
import Control.Monad.Fix
import Control.Monad.Zip
import Control.Lens as Lens
import Data.Binary as Binary
import Data.Bytes.Serial
import Data.Serialize as Cereal
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
import Data.Semigroup.Foldable
import qualified Data.Traversable.WithIndex as WithIndex
import qualified Data.Vector as V
import Linear.V
import Foreign.Storable (Storable)
import GHC.Arr (Ix(..))
import GHC.Generics (Generic, Generic1)
#if defined(MIN_VERSION_template_haskell)
import Language.Haskell.TH.Syntax (Lift)
#endif
import Linear.Metric
import Linear.Epsilon
import Linear.Vector
import Prelude hiding (sum)
import System.Random (Random(..), Uniform)
import System.Random.Stateful (UniformRange(..))
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif

import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U

-- $setup
-- >>> import Control.Applicative
-- >>> import Control.Lens
-- >>> import qualified Data.Foldable as F
-- >>> let sum xs = F.sum xs

-- | A 1-dimensional vector
--
-- >>> pure 1 :: V1 Int
-- V1 1
--
-- >>> V1 2 + V1 3
-- V1 5
--
-- >>> V1 2 * V1 3
-- V1 6
--
-- >>> sum (V1 2)
-- 2

--data V2 a = V2 !a !a deriving (Eq,Ord,Show,Read,Data)
newtype V1 a = V1 a
  deriving (Eq,Ord,Show,Read,Data,
            Functor,Traversable,
            Epsilon,Storable,NFData
           ,Generic,Generic1
#if defined(MIN_VERSION_template_haskell)
           ,Lift
#endif
           )

instance Foldable V1 where
  foldMap f (V1 a) = f a
#if MIN_VERSION_base(4,13,0)
  foldMap' f (V1 a) = f a
#endif
  null _ = False
  length _ = 1

instance Finite V1 where
  type Size V1 = 1
  toV (V1 a) = V (V.singleton a)
  fromV (V v) = V1 (v V.! 0)

instance Foldable1 V1 where
  foldMap1 f (V1 a) = f a
  {-# INLINE foldMap1 #-}

instance Traversable1 V1 where
  traverse1 f (V1 a) = V1 <$> f a
  {-# INLINE traverse1 #-}

instance Apply V1 where
  V1 f <.> V1 x = V1 (f x)
  {-# INLINE (<.>) #-}

instance Applicative V1 where
  pure = V1
  {-# INLINE pure #-}
  V1 f <*> V1 x = V1 (f x)
  {-# INLINE (<*>) #-}

instance Additive V1 where
  zero = pure 0
  {-# INLINE zero #-}
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

instance Bind V1 where
  V1 a >>- f = f a
  {-# INLINE (>>-) #-}

instance Monad V1 where
#if !(MIN_VERSION_base(4,11,0))
  return = V1
  {-# INLINE return #-}
#endif
  V1 a >>= f = f a
  {-# INLINE (>>=) #-}

instance Num a => Num (V1 a) where
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

instance Fractional a => Fractional (V1 a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Floating a => Floating (V1 a) where
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

instance Hashable a => Hashable (V1 a) where
  hash (V1 a) = hash a
  hashWithSalt s (V1 a) = s `hashWithSalt` a

instance Hashable1 V1 where
  liftHashWithSalt h s (V1 a) = h s a
  {-# INLINE liftHashWithSalt #-}

instance Metric V1 where
  dot (V1 a) (V1 b) = a * b
  {-# INLINE dot #-}

-- | A space that has at least 1 basis vector '_x'.
class R1 t where
  -- |
  -- >>> V1 2 ^._x
  -- 2
  --
  -- >>> V1 2 & _x .~ 3
  -- V1 3
  --
  _x :: Lens' (t a) a

ex :: R1 t => E t
ex = E _x

instance R1 V1 where
  _x f (V1 a) = V1 <$> f a
  {-# INLINE _x #-}

instance R1 Identity where
  _x f (Identity a) = Identity <$> f a
  {-# INLINE _x #-}

instance Distributive V1 where
  distribute f = V1 (fmap (\(V1 x) -> x) f)
  {-# INLINE distribute #-}

instance Ix a => Ix (V1 a) where
  {-# SPECIALISE instance Ix (V1 Int) #-}

  range (V1 l1, V1 u1) =
    [ V1 i1 | i1 <- range (l1,u1) ]
  {-# INLINE range #-}

  unsafeIndex (V1 l1,V1 u1) (V1 i1) = unsafeIndex (l1,u1) i1
  {-# INLINE unsafeIndex #-}

  inRange (V1 l1,V1 u1) (V1 i1) = inRange (l1,u1) i1
  {-# INLINE inRange #-}

instance Representable V1 where
  type Rep V1 = E V1
  tabulate f = V1 (f ex)
  {-# INLINE tabulate #-}
  index xs (E l) = view l xs
  {-# INLINE index #-}

instance WithIndex.FunctorWithIndex (E V1) V1 where
  imap f (V1 a) = V1 (f ex a)
  {-# INLINE imap #-}

instance WithIndex.FoldableWithIndex (E V1) V1 where
  ifoldMap f (V1 a) = f ex a
  {-# INLINE ifoldMap #-}

instance WithIndex.TraversableWithIndex (E V1) V1 where
  itraverse f (V1 a) = V1 <$> f ex a
  {-# INLINE itraverse #-}

#if !MIN_VERSION_lens(5,0,0)
instance Lens.FunctorWithIndex     (E V1) V1 where imap      = WithIndex.imap
instance Lens.FoldableWithIndex    (E V1) V1 where ifoldMap  = WithIndex.ifoldMap
instance Lens.TraversableWithIndex (E V1) V1 where itraverse = WithIndex.itraverse
#endif

type instance Index (V1 a) = E V1
type instance IxValue (V1 a) = a

instance Ixed (V1 a) where
  ix i = el i
  {-# INLINE ix #-}

instance Each (V1 a) (V1 b) a b where
  each f (V1 x) = V1 <$> f x
  {-# INLINE each #-}

newtype instance U.Vector    (V1 a) = V_V1  (U.Vector    a)
newtype instance U.MVector s (V1 a) = MV_V1 (U.MVector s a)
instance U.Unbox a => U.Unbox (V1 a)

instance U.Unbox a => M.MVector U.MVector (V1 a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  basicLength (MV_V1 v) = M.basicLength v
  basicUnsafeSlice m n (MV_V1 v) = MV_V1 (M.basicUnsafeSlice m n v)
  basicOverlaps (MV_V1 v) (MV_V1 u) = M.basicOverlaps v u
  basicUnsafeNew n = liftM MV_V1 (M.basicUnsafeNew n)
  basicUnsafeRead (MV_V1 v) i = liftM V1 (M.basicUnsafeRead v i)
  basicUnsafeWrite (MV_V1 v) i (V1 x) = M.basicUnsafeWrite v i x
  basicInitialize (MV_V1 v) = M.basicInitialize v
  {-# INLINE basicInitialize #-}

instance U.Unbox a => G.Vector U.Vector (V1 a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze (MV_V1 v) = liftM V_V1 (G.basicUnsafeFreeze v)
  basicUnsafeThaw (V_V1 v) = liftM MV_V1 (G.basicUnsafeThaw v)
  basicLength (V_V1 v) = G.basicLength v
  basicUnsafeSlice m n (V_V1 v) = V_V1 (G.basicUnsafeSlice m n v)
  basicUnsafeIndexM (V_V1 v) i = liftM V1 (G.basicUnsafeIndexM v i)

instance MonadZip V1 where
  mzip (V1 a) (V1 b) = V1 (a, b)
  mzipWith f (V1 a) (V1 b) = V1 (f a b)
  munzip (V1 (a,b)) = (V1 a, V1 b)

instance MonadFix V1 where
  mfix f = V1 (let V1 a = f a in a)

instance Bounded a => Bounded (V1 a) where
  minBound = pure minBound
  {-# INLINE minBound #-}
  maxBound = pure maxBound
  {-# INLINE maxBound #-}

instance Serial1 V1 where
  serializeWith f (V1 a) = f a
  deserializeWith m = V1 `liftM` m

instance Serial a => Serial (V1 a) where
  serialize (V1 a) = serialize a
  deserialize = V1 `liftM` deserialize

instance Binary a => Binary (V1 a) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance Serialize a => Serialize (V1 a) where
  put = serializeWith Cereal.put
  get = deserializeWith Cereal.get

instance Random a => Random (V1 a) where
  random g = case random g of (a, g') -> (V1 a, g')
  randoms g = V1 <$> randoms g
  randomR (V1 a, V1 b) g = case randomR (a, b) g of (a', g') -> (V1 a', g')
  randomRs (V1 a, V1 b) g = V1 <$> randomRs (a, b) g

instance Uniform a => Uniform (V1 a) where

instance UniformRange a => UniformRange (V1 a) where
  uniformRM (V1 a, V1 b) g = V1 <$> uniformRM (a, b) g

instance Eq1 V1 where
  liftEq f (V1 a) (V1 b) = f a b
instance Ord1 V1 where
  liftCompare f (V1 a) (V1 b) = f a b
instance Show1 V1 where
  liftShowsPrec f _ d (V1 a) = showParen (d >= 10) $ showString "V1 " . f d a
instance Read1 V1 where
  liftReadsPrec f _ = readsData $ readsUnaryWith f "V1" V1

instance Field1 (V1 a) (V1 b) a b where
  _1 f (V1 x) = V1 <$> f x

instance Semigroup a => Semigroup (V1 a) where
 (<>) = liftA2 (<>)

instance Monoid a => Monoid (V1 a) where
  mempty = pure mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = liftA2 mappend
#endif


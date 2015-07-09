{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
#define USE_TYPE_LITS 1
#endif
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}

#ifndef MIN_VERSION_reflection
#define MIN_VERSION_reflection(x,y,z) 1
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
-- n-D Vectors
----------------------------------------------------------------------------

module Linear.V
  ( V(V,toVector)
#ifdef MIN_VERSION_template_haskell
  , int
#endif
  , dim
  , Dim(..)
  , reifyDim
  , reifyVector
#if MIN_VERSION_reflection(2,0,0)
  , reifyDimNat
  , reifyVectorNat
#endif
  , fromVector
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.DeepSeq (NFData)
import Control.Monad.Fix
import Control.Monad.Zip
import Control.Lens as Lens
import Data.Binary as Binary
import Data.Bytes.Serial
import Data.Data
import Data.Distributive
import Data.Foldable as Foldable
import Data.Functor.Bind
import Data.Functor.Classes
import Data.Functor.Rep as Rep
#if __GLASGOW_HASKELL__ < 708
import Data.Proxy
#endif
import Data.Reflection as R
import Data.Serialize as Cereal
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable (sequenceA)
#endif
import Data.Vector as V
import Foreign.Ptr
import Foreign.Storable
#ifdef USE_TYPE_LITS
import GHC.TypeLits
#endif
#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif
#if __GLASGOW_HASKELL__ >= 707
import GHC.Generics (Generic1)
#endif
#if !(MIN_VERSION_reflection(1,3,0)) && defined(MIN_VERSION_template_haskell)
import Language.Haskell.TH
#endif
import Linear.Epsilon
import Linear.Metric
import Linear.Vector


#ifdef HLINT
{-# ANN module "hlint: ignore Eta reduce" #-}
#endif

class Dim n where
  reflectDim :: p n -> Int

#if __GLASGOW_HASKELL__ >= 707
type role V nominal representational
#endif


newtype V n a = V { toVector :: V.Vector a } deriving (Eq,Ord,Show,Read,Typeable,NFData
                                                      , Generic
-- GHC bug: https://ghc.haskell.org/trac/ghc/ticket/8468
#if __GLASGOW_HASKELL__ >= 707
                                                      ,Generic1
#endif
                                                      )
dim :: forall n a. Dim n => V n a -> Int
dim _ = reflectDim (Proxy :: Proxy n)
{-# INLINE dim #-}

#ifdef USE_TYPE_LITS
instance KnownNat n => Dim (n :: Nat) where
  reflectDim = fromInteger . natVal
  {-# INLINE reflectDim #-}
#endif

data ReifiedDim (s :: *)

retagDim :: (Proxy s -> a) -> proxy (ReifiedDim s) -> a
retagDim f _ = f Proxy
{-# INLINE retagDim #-}

instance Reifies s Int => Dim (ReifiedDim s) where
  reflectDim = retagDim reflect
  {-# INLINE reflectDim #-}

#if (MIN_VERSION_reflection(2,0,0)) && __GLASGOW_HASKELL__ >= 708

reifyDimNat :: Int -> (forall (n :: Nat). KnownNat n => Proxy n -> r) -> r
reifyDimNat i f = R.reifyNat (fromIntegral i) f
{-# INLINE reifyDimNat #-}

reifyVectorNat :: forall a r. Vector a -> (forall (n :: Nat). KnownNat n => V n a -> r) -> r
reifyVectorNat v f = reifyNat (fromIntegral $ V.length v) $ \(Proxy :: Proxy n) -> f (V v :: V n a)
{-# INLINE reifyVectorNat #-}

#endif

reifyDim :: Int -> (forall (n :: *). Dim n => Proxy n -> r) -> r
reifyDim i f = R.reify i (go f) where
  go :: Reifies n Int => (Proxy (ReifiedDim n) -> a) -> proxy n -> a
  go g _ = g Proxy
{-# INLINE reifyDim #-}

reifyVector :: forall a r. Vector a -> (forall (n :: *). Dim n => V n a -> r) -> r
reifyVector v f = reifyDim (V.length v) $ \(Proxy :: Proxy n) -> f (V v :: V n a)
{-# INLINE reifyVector #-}

instance Dim n => Dim (V n a) where
  reflectDim _ = reflectDim (Proxy :: Proxy n)
  {-# INLINE reflectDim #-}

instance Functor (V n) where
  fmap f (V as) = V (fmap f as)
  {-# INLINE fmap #-}

instance FunctorWithIndex Int (V n) where
  imap f (V as) = V (Lens.imap f as)
  {-# INLINE imap #-}

instance Foldable (V n) where
  fold (V as) = fold as
  {-# INLINE fold #-}
  foldMap f (V as) = foldMap f as
  {-# INLINE foldMap #-}
  foldr f z (V as) = V.foldr f z as
  {-# INLINE foldr #-}
  foldl f z (V as) = V.foldl f z as
  {-# INLINE foldl #-}
  foldr' f z (V as) = V.foldr' f z as
  {-# INLINE foldr' #-}
  foldl' f z (V as) = V.foldl' f z as
  {-# INLINE foldl' #-}
  foldr1 f (V as) = V.foldr1 f as
  {-# INLINE foldr1 #-}
  foldl1 f (V as) = V.foldl1 f as
  {-# INLINE foldl1 #-}

#if __GLASGOW_HASKELL__ >= 710
  length (V as) = V.length as
  {-# INLINE length #-}
  null (V as) = V.null as
  {-# INLINE null #-}
  toList (V as) = V.toList as
  {-# INLINE toList #-}
  elem a (V as) = V.elem a as
  {-# INLINE elem #-}
  maximum (V as) = V.maximum as
  {-# INLINE maximum #-}
  minimum (V as) = V.minimum as
  {-# INLINE minimum #-}
  sum (V as) = V.sum as
  {-# INLINE sum #-}
  product (V as) = V.product as
  {-# INLINE product #-}
#endif
  

instance FoldableWithIndex Int (V n) where
  ifoldMap f (V as) = ifoldMap f as
  {-# INLINE ifoldMap #-}

instance Traversable (V n) where
  traverse f (V as) = V <$> traverse f as
  {-# INLINE traverse #-}

instance TraversableWithIndex Int (V n) where
  itraverse f (V as) = V <$> itraverse f as
  {-# INLINE itraverse #-}

instance Apply (V n) where
  V as <.> V bs = V (V.zipWith id as bs)
  {-# INLINE (<.>) #-}

instance Dim n => Applicative (V n) where
  pure = V . V.replicate (reflectDim (Proxy :: Proxy n))
  {-# INLINE pure #-}

  V as <*> V bs = V (V.zipWith id as bs)
  {-# INLINE (<*>) #-}

instance Bind (V n) where
  V as >>- f = V $ generate (V.length as) $ \i ->
    toVector (f (as `unsafeIndex` i)) `unsafeIndex` i
  {-# INLINE (>>-) #-}

instance Dim n => Monad (V n) where
  return = V . V.replicate (reflectDim (Proxy :: Proxy n))
  {-# INLINE return #-}
  V as >>= f = V $ generate (reflectDim (Proxy :: Proxy n)) $ \i ->
    toVector (f (as `unsafeIndex` i)) `unsafeIndex` i
  {-# INLINE (>>=) #-}

instance Dim n => Additive (V n) where
  zero = pure 0
  {-# INLINE zero #-}
  liftU2 f (V as) (V bs) = V (V.zipWith f as bs)
  {-# INLINE liftU2 #-}
  liftI2 f (V as) (V bs) = V (V.zipWith f as bs)
  {-# INLINE liftI2 #-}

instance (Dim n, Num a) => Num (V n a) where
  V as + V bs = V $ V.zipWith (+) as bs
  {-# INLINE (+) #-}
  V as - V bs = V $ V.zipWith (-) as bs
  {-# INLINE (-) #-}
  V as * V bs = V $ V.zipWith (*) as bs
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance (Dim n, Fractional a) => Fractional (V n a) where
  recip = fmap recip
  {-# INLINE recip #-}
  V as / V bs = V $ V.zipWith (/) as bs
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance (Dim n, Floating a) => Floating (V n a) where
    pi = pure pi
    {-# INLINE pi #-}
    exp = fmap exp
    {-# INLINE exp #-}
    sqrt = fmap sqrt
    {-# INLINE sqrt #-}
    log = fmap log
    {-# INLINE log #-}
    V as ** V bs = V $ V.zipWith (**) as bs
    {-# INLINE (**) #-}
    logBase (V as) (V bs) = V $ V.zipWith logBase as bs
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

instance Dim n => Distributive (V n) where
  distribute f = V $ V.generate (reflectDim (Proxy :: Proxy n)) $ \i -> fmap (\(V v) -> unsafeIndex v i) f
  {-# INLINE distribute #-}

instance (Dim n, Storable a) => Storable (V n a) where
  sizeOf _ = reflectDim (Proxy :: Proxy n) * sizeOf (undefined:: a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: a)
  {-# INLINE alignment #-}
  poke ptr (V xs) = Foldable.forM_ [0..reflectDim (Proxy :: Proxy n)-1] $ \i ->
    pokeElemOff ptr' i (unsafeIndex xs i)
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = V <$> generateM (reflectDim (Proxy :: Proxy n)) (peekElemOff ptr')
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

instance (Dim n, Epsilon a) => Epsilon (V n a) where
  nearZero = nearZero . quadrance
  {-# INLINE nearZero #-}

instance Dim n => Metric (V n) where
  dot (V a) (V b) = V.sum $ V.zipWith (*) a b
  {-# INLINE dot #-}

-- TODO: instance (Dim n, Ix a) => Ix (V n a)

fromVector :: forall n a. Dim n => Vector a -> Maybe (V n a)
fromVector v
  | V.length v == reflectDim (Proxy :: Proxy n) = Just (V v)
  | otherwise                                   = Nothing

#if !(MIN_VERSION_reflection(1,3,0)) && defined(MIN_VERSION_template_haskell)
data Z  -- 0
data D  (n :: *) -- 2n
data SD (n :: *) -- 2n+1
data PD (n :: *) -- 2n-1

instance Reifies Z Int where
  reflect _ = 0
  {-# INLINE reflect #-}

retagD :: (Proxy n -> a) -> proxy (D n) -> a
retagD f _ = f Proxy
{-# INLINE retagD #-}

retagSD :: (Proxy n -> a) -> proxy (SD n) -> a
retagSD f _ = f Proxy
{-# INLINE retagSD #-}

retagPD :: (Proxy n -> a) -> proxy (PD n) -> a
retagPD f _ = f Proxy
{-# INLINE retagPD #-}

instance Reifies n Int => Reifies (D n) Int where
  reflect = (\n -> n+n) <$> retagD reflect
  {-# INLINE reflect #-}

instance Reifies n Int => Reifies (SD n) Int where
  reflect = (\n -> n+n+1) <$> retagSD reflect
  {-# INLINE reflect #-}

instance Reifies n Int => Reifies (PD n) Int where
  reflect = (\n -> n+n-1) <$> retagPD reflect
  {-# INLINE reflect #-}

-- | This can be used to generate a template haskell splice for a type level version of a given 'int'.
--
-- This does not use GHC TypeLits, instead it generates a numeric type by hand similar to the ones used
-- in the \"Functional Pearl: Implicit Dimurations\" paper by Oleg Kiselyov and Chung-Chieh Shan.
int :: Int -> TypeQ
int n = case quotRem n 2 of
  (0, 0) -> conT ''Z
  (q,-1) -> conT ''PD `appT` int q
  (q, 0) -> conT ''D  `appT` int q
  (q, 1) -> conT ''SD `appT` int q
  _     -> error "ghc is bad at math"
#endif

instance Dim n => Representable (V n) where
  type Rep (V n) = Int
  tabulate = V . generate (reflectDim (Proxy :: Proxy n))
  {-# INLINE tabulate #-}
  index (V xs) i = xs V.! i
  {-# INLINE index #-}

type instance Index (V n a) = Int
type instance IxValue (V n a) = a

instance Ixed (V n a) where
  ix i f (V as)
     | i < 0 || i >= V.length as = pure $ V as
     | otherwise = f (as ! i) <&> \a -> V $ as V.// [(i, a)]
  {-# INLINE ix #-}

instance Dim n => MonadZip (V n) where
  mzip (V as) (V bs) = V $ V.zip as bs
  mzipWith f (V as) (V bs) = V $ V.zipWith f as bs

instance Dim n => MonadFix (V n) where
  mfix f = tabulate $ \r -> let a = Rep.index (f a) r in a

instance Each (V n a) (V n b) a b where
  each = traverse
  {-# INLINE each #-}

instance (Bounded a, Dim n) => Bounded (V n a) where
  minBound = pure minBound
  {-# INLINE minBound #-}
  maxBound = pure maxBound
  {-# INLINE maxBound #-}

vConstr :: Constr
vConstr = mkConstr vDataType "variadic" [] Prefix
{-# NOINLINE vConstr #-}

vDataType :: DataType
vDataType = mkDataType "Linear.V.V" [vConstr]
{-# NOINLINE vDataType #-}

#if __GLASGOW_HASKELL__ >= 708
#define Typeable1 Typeable
#endif

instance (Typeable1 (V n), Typeable (V n a), Dim n, Data a) => Data (V n a) where
  gfoldl f z (V as) = z (V . fromList) `f` V.toList as
  toConstr _ = vConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z (V . fromList))
    _ -> error "gunfold"
  dataTypeOf _ = vDataType
  dataCast1 f = gcast1 f

instance Dim n => Serial1 (V n) where
  serializeWith = traverse_
  deserializeWith f = sequenceA $ pure f

instance (Dim n, Serial a) => Serial (V n a) where
  serialize = traverse_ serialize
  deserialize = sequenceA $ pure deserialize

instance (Dim n, Binary a) => Binary (V n a) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance (Dim n, Serialize a) => Serialize (V n a) where
  put = serializeWith Cereal.put
  get = deserializeWith Cereal.get

instance Dim n => Eq1 (V n) where eq1 = (==)
instance Dim n => Ord1 (V n) where compare1 = compare
instance Dim n => Show1 (V n) where showsPrec1 = showsPrec
instance Dim n => Read1 (V n) where readsPrec1 = readsPrec

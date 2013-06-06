{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#define USE_TYPE_LITS 1
#endif

module Linear.V
  ( V(toVector)
  , int
  , dim
  , Dim(..)
  , reifyDim
  , reifyVector
  , fromVector
  ) where

import Control.Applicative
import Data.Distributive
import Data.Foldable as Foldable
import Data.Functor.Bind
import Data.Proxy
import Data.Reflection as R
import Data.Traversable
import Data.Vector as V
import Foreign.Ptr
import Foreign.Storable
#ifdef USE_TYPE_LITS
import GHC.TypeLits
#endif
#if !(MIN_VERSION_reflection(1,3,0))
import Language.Haskell.TH
#endif
import Linear.Core
import Linear.Epsilon
import Linear.Metric
import Linear.Vector

class Dim n where
  reflectDim :: p n -> Int

newtype V n a = V { toVector :: V.Vector a } deriving (Eq,Ord,Show,Read)

dim :: forall n a. Dim n => V n a -> Int
dim _ = reflectDim (Proxy :: Proxy n)
{-# INLINE dim #-}

#ifdef USE_TYPE_LITS
instance SingRep n Integer => Dim (n :: Nat) where
  reflectDim _ = fromInteger $ withSing $ \(x :: Sing n) -> fromSing x
  {-# INLINE reflectDim #-}
#endif

data ReifiedDim (s :: *)

retagDim :: (Proxy s -> a) -> proxy (ReifiedDim s) -> a
retagDim f _ = f Proxy
{-# INLINE retagDim #-}

instance Reifies s Int => Dim (ReifiedDim s) where
  reflectDim = retagDim reflect
  {-# INLINE reflectDim #-}

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

instance Foldable (V n) where
  foldMap f (V as) = foldMap f as
  {-# INLINE foldMap #-}

instance Traversable (V n) where
  traverse f (V as) = V <$> traverse f as
  {-# INLINE traverse #-}

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

instance Dim n => Core (V n) where
  core f = V $ generate (reflectDim (Proxy :: Proxy n)) $ \i -> f $ \g (V v) ->
    (\a -> V $ v V.// [(i,a)]) <$> g (unsafeIndex v i)
  {-# INLINE core #-}

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

#if !(MIN_VERSION_reflection(1,3,0))
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


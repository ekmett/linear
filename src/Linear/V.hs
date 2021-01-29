{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE RoleAnnotations #-}
#define USE_TYPE_LITS 1
#endif
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}

#ifndef MIN_VERSION_hashable
#define MIN_VERSION_hashable(x,y,z) 1
#endif

#ifndef MIN_VERSION_reflection
#define MIN_VERSION_reflection(x,y,z) 1
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
#if (MIN_VERSION_reflection(2,0,0)) && __GLASGOW_HASKELL__ >= 708
  , reifyDimNat
  , reifyVectorNat
#endif
  , fromVector
#if __GLASGOW_HASKELL__ >= 707
  , Finite(..)
  , _V, _V'
#endif
  ) where

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.State
import Control.Monad.Zip
import Control.Lens as Lens
import Data.Binary as Binary
import Data.Bytes.Serial
#if __GLASGOW_HASKELL__ >= 707
import Data.Complex
#endif
import Data.Data
import Data.Distributive
import Data.Foldable as Foldable
import Data.Functor.Bind
import Data.Functor.Classes
import Data.Functor.Rep as Rep
import Data.Hashable
#if (MIN_VERSION_hashable(1,2,5))
import Data.Hashable.Lifted
#endif
#if __GLASGOW_HASKELL__ < 708
import Data.Proxy
#endif
import Data.Reflection as R
import Data.Serialize as Cereal
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable (sequenceA)
#endif
import Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Mutable as M
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
#if (MIN_VERSION_transformers(0,5,0)) || !(MIN_VERSION_transformers(0,4,0))
import Prelude as P
#endif
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import System.Random

class Dim n where
  reflectDim :: p n -> Int

#if __GLASGOW_HASKELL__ >= 707
type role V nominal representational

class Finite v where
  type Size (v :: * -> *) :: Nat -- this should allow kind k, for Reifies k Int
  toV :: v a -> V (Size v) a
  default toV :: Foldable v => v a -> V (Size v) a
  toV = V . V.fromList . Foldable.toList
  fromV :: V (Size v) a -> v a

instance Finite Complex where
  type Size Complex = 2
  toV (a :+ b) = V (V.fromListN 2 [a, b])
  fromV (V v) = (v V.! 0) :+ (v V.! 1)

_V :: (Finite u, Finite v) => Iso (V (Size u) a) (V (Size v) b) (u a) (v b)
_V = iso fromV toV

_V' :: Finite v => Iso (V (Size v) a) (V (Size v) b) (v a) (v b)
_V' = iso fromV toV

instance Finite (V (n :: Nat)) where
  type Size (V n) = n
  toV = id
  fromV = id
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

instance (Dim n, Random a) => Random (V n a) where
  random = runState (V <$> V.replicateM (reflectDim (Proxy :: Proxy n)) (state random))
  randomR (V ls,V hs) = runState (V <$> V.zipWithM (\l h -> state $ randomR (l,h)) ls hs)

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
  go :: (Proxy (ReifiedDim n) -> a) -> proxy n -> a
  go g _ = g Proxy
{-# INLINE reifyDim #-}

reifyVector :: forall a r. Vector a -> (forall (n :: *). Dim n => V n a -> r) -> r
reifyVector v f = reifyDim (V.length v) $ \(Proxy :: Proxy n) -> f (V v :: V n a)
{-# INLINE reifyVector #-}

instance Dim n => Dim (V n a) where
  reflectDim _ = reflectDim (Proxy :: Proxy n)
  {-# INLINE reflectDim #-}

instance (Dim n, Semigroup a) => Semigroup (V n a) where
 (<>) = liftA2 (<>)

instance (Dim n, Monoid a) => Monoid (V n a) where
  mempty = pure mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = liftA2 mappend
#endif

instance Functor (V n) where
  fmap f (V as) = V (fmap f as)
  {-# INLINE fmap #-}

instance FunctorWithIndex Int (V n) where
  imap f (V as) = V (Lens.imap f as)
  {-# INLINE imap #-}

instance Foldable (V n) where
  fold (V as) = fold as
  {-# INLINE fold #-}
  foldMap f (V as) = Foldable.foldMap f as
  {-# INLINE foldMap #-}
  foldr f z (V as) = V.foldr f z as
  {-# INLINE foldr #-}
  foldl f z (V as) = V.foldl f z as
  {-# INLINE foldl #-}
#if __GLASGOW_HASKELL__ >= 706
  foldr' f z (V as) = V.foldr' f z as
  {-# INLINE foldr' #-}
  foldl' f z (V as) = V.foldl' f z as
  {-# INLINE foldl' #-}
#endif
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

instance Hashable a => Hashable (V n a) where
  hashWithSalt s0 (V v) =
    V.foldl' (\s a -> s `hashWithSalt` a) s0 v
      `hashWithSalt` V.length v

#if (MIN_VERSION_hashable(1,2,5))
instance Dim n => Hashable1 (V n) where
  liftHashWithSalt h s0 (V v) =
    V.foldl' (\s a -> h s a) s0 v
      `hashWithSalt` V.length v
  {-# INLINE liftHashWithSalt #-}
#endif

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
  ix i f v@(V as)
     | i < 0 || i >= V.length as = pure v
     | otherwise = vLens i f v
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

#if (MIN_VERSION_transformers(0,5,0)) || !(MIN_VERSION_transformers(0,4,0))
instance Eq1 (V n) where
  liftEq f0 (V as0) (V bs0) = go f0 (V.toList as0) (V.toList bs0) where
    go _ [] [] = True
    go f (a:as) (b:bs) = f a b && go f as bs
    go _ _ _ = False

instance Ord1 (V n) where
  liftCompare f0 (V as0) (V bs0) = go f0 (V.toList as0) (V.toList bs0) where
    go f (a:as) (b:bs) = f a b `mappend` go f as bs
    go _ [] [] = EQ
    go _ _  [] = GT
    go _ [] _  = LT

instance Show1 (V n) where
  liftShowsPrec _ g d (V as) = showParen (d > 10) $ showString "V " . g (V.toList as)

instance Dim n => Read1 (V n) where
  liftReadsPrec _ g d = readParen (d > 10) $ \r ->
    [ (V (V.fromList as), r2)
    | ("V",r1) <- lex r
    , (as, r2) <- g r1
    , P.length as == reflectDim (Proxy :: Proxy n)
    ]
#else
instance Dim n => Eq1 (V n) where eq1 = (==)
instance Dim n => Ord1 (V n) where compare1 = compare
instance Dim n => Show1 (V n) where showsPrec1 = showsPrec
instance Dim n => Read1 (V n) where readsPrec1 = readsPrec
#endif


data instance U.Vector    (V n a) =  V_VN {-# UNPACK #-} !Int !(U.Vector    a)
data instance U.MVector s (V n a) = MV_VN {-# UNPACK #-} !Int !(U.MVector s a)
instance (Dim n, U.Unbox a) => U.Unbox (V n a)

instance (Dim n, U.Unbox a) => M.MVector U.MVector (V n a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  basicLength (MV_VN n _) = n
  basicUnsafeSlice m n (MV_VN _ v) = MV_VN n (M.basicUnsafeSlice (d*m) (d*n) v)
    where d = reflectDim (Proxy :: Proxy n)
  basicOverlaps (MV_VN _ v) (MV_VN _ u) = M.basicOverlaps v u
  basicUnsafeNew n = liftM (MV_VN n) (M.basicUnsafeNew (d*n))
    where d = reflectDim (Proxy :: Proxy n)
  basicUnsafeRead (MV_VN _ v) i =
    liftM V $ V.generateM d (\j -> M.basicUnsafeRead v (d*i+j))
    where d = reflectDim (Proxy :: Proxy n)
  basicUnsafeWrite (MV_VN _ v0) i (V vn0) = let d0 = V.length vn0 in go v0 vn0 d0 (d0*i) 0
   where
    go v vn d o j
      | j >= d = return ()
      | otherwise = do
        a <- G.basicUnsafeIndexM vn j
        M.basicUnsafeWrite v o a
        go v vn d (o+1) (j+1)
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_VN _ v) = M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif

instance (Dim n, U.Unbox a) => G.Vector U.Vector (V n a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze (MV_VN n v) = liftM ( V_VN n) (G.basicUnsafeFreeze v)
  basicUnsafeThaw   ( V_VN n v) = liftM (MV_VN n) (G.basicUnsafeThaw   v)
  basicLength       ( V_VN n _) = n
  basicUnsafeSlice m n (V_VN _ v) = V_VN n (G.basicUnsafeSlice (d*m) (d*n) v)
    where d = reflectDim (Proxy :: Proxy n)
  basicUnsafeIndexM (V_VN _ v) i =
    liftM V $ V.generateM d (\j -> G.basicUnsafeIndexM v (d*i+j))
    where d = reflectDim (Proxy :: Proxy n)

vLens :: Int -> Lens' (V n a) a
vLens i = \f (V v) -> f (v V.! i) <&> \a -> V (v V.// [(i, a)])
{-# INLINE vLens #-}

#ifdef USE_TYPE_LITS
instance ( 1 <= n) => Field1  (V n a) (V n a) a a where _1  = vLens  0
instance ( 2 <= n) => Field2  (V n a) (V n a) a a where _2  = vLens  1
instance ( 3 <= n) => Field3  (V n a) (V n a) a a where _3  = vLens  2
instance ( 4 <= n) => Field4  (V n a) (V n a) a a where _4  = vLens  3
instance ( 5 <= n) => Field5  (V n a) (V n a) a a where _5  = vLens  4
instance ( 6 <= n) => Field6  (V n a) (V n a) a a where _6  = vLens  5
instance ( 7 <= n) => Field7  (V n a) (V n a) a a where _7  = vLens  6
instance ( 8 <= n) => Field8  (V n a) (V n a) a a where _8  = vLens  7
instance ( 9 <= n) => Field9  (V n a) (V n a) a a where _9  = vLens  8
instance (10 <= n) => Field10 (V n a) (V n a) a a where _10 = vLens  9
instance (11 <= n) => Field11 (V n a) (V n a) a a where _11 = vLens 10
instance (12 <= n) => Field12 (V n a) (V n a) a a where _12 = vLens 11
instance (13 <= n) => Field13 (V n a) (V n a) a a where _13 = vLens 12
instance (14 <= n) => Field14 (V n a) (V n a) a a where _14 = vLens 13
instance (15 <= n) => Field15 (V n a) (V n a) a a where _15 = vLens 14
instance (16 <= n) => Field16 (V n a) (V n a) a a where _16 = vLens 15
instance (17 <= n) => Field17 (V n a) (V n a) a a where _17 = vLens 16
instance (18 <= n) => Field18 (V n a) (V n a) a a where _18 = vLens 17
instance (19 <= n) => Field19 (V n a) (V n a) a a where _19 = vLens 18
#endif

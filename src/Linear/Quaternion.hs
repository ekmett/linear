{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Quaternions
----------------------------------------------------------------------------
module Linear.Quaternion
  ( Quaternion(..)
  , Complicated(..)
  , Hamiltonian(..)
  , ee, ei, ej, ek
  , slerp
  , asinq
  , acosq
  , atanq
  , asinhq
  , acoshq
  , atanhq
  , absi
  , pow
  , rotate
  , axisAngle
  ) where

import Control.Applicative
import Control.DeepSeq (NFData(rnf))
import Control.Monad (liftM)
import Control.Monad.Fix
import Control.Monad.Zip
import Control.Lens hiding ((<.>))
import Data.Bytes.Serial
import Data.Complex (Complex((:+)))
import Data.Data
import Data.Distributive
import Data.Foldable
import Data.Functor.Bind
import Data.Functor.Classes
import Data.Functor.Rep
import Data.Hashable
import GHC.Arr (Ix(..))
import qualified Data.Foldable as F
import Data.Monoid
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
import Data.Binary as Binary
import Data.Serialize as Cereal
#endif
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U
import Linear.Epsilon
import Linear.Conjugate
import Linear.Metric
import Linear.V3
import Linear.Vector
import Prelude hiding (any)

{-# ANN module "HLint: ignore Reduce duplication" #-}

-- | Quaternions
data Quaternion a = Quaternion !a {-# UNPACK #-}!(V3 a)
                    deriving (Eq,Ord,Read,Show,Data,Typeable
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
                             ,Generic
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
                             ,Generic1
#endif
                             )

instance Functor Quaternion where
  fmap f (Quaternion e v) = Quaternion (f e) (fmap f v)
  {-# INLINE fmap #-}
  a <$ _ = Quaternion a (V3 a a a)
  {-# INLINE (<$) #-}

instance Apply Quaternion where
  Quaternion f fv <.> Quaternion a v = Quaternion (f a) (fv <.> v)
  {-# INLINE (<.>) #-}

instance Applicative Quaternion where
  pure a = Quaternion a (pure a)
  {-# INLINE pure #-}
  Quaternion f fv <*> Quaternion a v = Quaternion (f a) (fv <*> v)
  {-# INLINE (<*>) #-}

instance Additive Quaternion where
  zero = pure 0
  {-# INLINE zero #-}
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

instance Bind Quaternion where
  Quaternion a (V3 b c d) >>- f = Quaternion a' (V3 b' c' d') where
    Quaternion a' _          = f a
    Quaternion _ (V3 b' _ _) = f b
    Quaternion _ (V3 _ c' _) = f c
    Quaternion _ (V3 _ _ d') = f d
  {-# INLINE (>>-) #-}

instance Monad Quaternion where
  return = pure
  {-# INLINE return #-}
  -- the diagonal of a sedenion is super useful!
  Quaternion a (V3 b c d) >>= f = Quaternion a' (V3 b' c' d') where
    Quaternion a' _          = f a
    Quaternion _ (V3 b' _ _) = f b
    Quaternion _ (V3 _ c' _) = f c
    Quaternion _ (V3 _ _ d') = f d
  {-# INLINE (>>=) #-}

instance Ix a => Ix (Quaternion a) where
    {-# SPECIALISE instance Ix (Quaternion Int) #-}

    range (Quaternion l1 l2, Quaternion u1 u2) =
      [ Quaternion i1 i2 | i1 <- range (l1,u1), i2 <- range (l2,u2) ]
    {-# INLINE range #-}

    unsafeIndex (Quaternion l1 l2, Quaternion u1 u2) (Quaternion i1 i2) =
      unsafeIndex (l1,u1) i1 * unsafeRangeSize (l2,u2) + unsafeIndex (l2,u2) i2
    {-# INLINE unsafeIndex #-}

    inRange (Quaternion l1 l2, Quaternion u1 u2) (Quaternion i1 i2) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2
    {-# INLINE inRange #-}

instance Representable Quaternion where
  type Rep Quaternion = E Quaternion
  tabulate f = Quaternion (f ee) (V3 (f ei) (f ej) (f ek))
  {-# INLINE tabulate #-}
  index xs (E l) = view l xs
  {-# INLINE index #-}

instance FunctorWithIndex (E Quaternion) Quaternion where
  imap f (Quaternion a (V3 b c d)) = Quaternion (f ee a) $ V3 (f ei b) (f ej c) (f ek d)
  {-# INLINE imap #-}

instance FoldableWithIndex (E Quaternion) Quaternion where
  ifoldMap f (Quaternion a (V3 b c d)) = f ee a `mappend` f ei b `mappend` f ej c `mappend` f ek d
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex (E Quaternion) Quaternion where
  itraverse f (Quaternion a (V3 b c d)) = Quaternion <$> f ee a <*> (V3 <$> f ei b <*> f ej c <*> f ek d)
  {-# INLINE itraverse #-}

type instance Index (Quaternion a) = E Quaternion
type instance IxValue (Quaternion a) = a

instance Ixed (Quaternion a) where
  ix = el
  {-# INLINE ix #-}

instance Each (Quaternion a) (Quaternion b) a b where
  each = traverse
  {-# INLINE each #-}

instance Foldable Quaternion where
  foldMap f (Quaternion e v) = f e `mappend` foldMap f v
  {-# INLINE foldMap #-}
  foldr f z (Quaternion e v) = f e (F.foldr f z v)
  {-# INLINE foldr #-}

instance Traversable Quaternion where
  traverse f (Quaternion e v) = Quaternion <$> f e <*> traverse f v
  {-# INLINE traverse #-}

instance Storable a => Storable (Quaternion a) where
  sizeOf _ = 4 * sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (Quaternion e v) = poke (castPtr ptr) e >>
                              poke (castPtr (ptr `plusPtr` sz)) v
    where sz = sizeOf (undefined::a)
  {-# INLINE poke #-}
  peek ptr = Quaternion <$> peek (castPtr ptr)
                        <*> peek (castPtr (ptr `plusPtr` sz))
    where sz = sizeOf (undefined::a)
  {-# INLINE peek #-}

instance RealFloat a => Num (Quaternion a) where
  {-# SPECIALIZE instance Num (Quaternion Float) #-}
  {-# SPECIALIZE instance Num (Quaternion Double) #-}
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  Quaternion s1 v1 * Quaternion s2 v2 = Quaternion (s1*s2 - (v1 `dot` v2)) $
                                        (v1 `cross` v2) + s1*^v2 + s2*^v1
  {-# INLINE (*) #-}
  fromInteger x = Quaternion (fromInteger x) 0
  {-# INLINE fromInteger #-}
  abs z = Quaternion (norm z) 0
  {-# INLINE abs #-}
  signum q@(Quaternion e (V3 i j k))
    | m == 0.0 = q
    | not (isInfinite m || isNaN m) = q ^/ sqrt m
    | any isNaN q = qNaN
    | not (ii || ij || ik) = Quaternion 1 (V3 0 0 0)
    | not (ie || ij || ik) = Quaternion 0 (V3 1 0 0)
    | not (ie || ii || ik) = Quaternion 0 (V3 0 1 0)
    | not (ie || ii || ij) = Quaternion 0 (V3 0 0 1)
    | otherwise = qNaN
    where
      m = quadrance q
      ie = isInfinite e
      ii = isInfinite i
      ij = isInfinite j
      ik = isInfinite k
  {-# INLINE signum #-}

instance Hashable a => Hashable (Quaternion a) where
  hashWithSalt s (Quaternion a b) = s `hashWithSalt` a `hashWithSalt` b
  {-# INLINE hashWithSalt #-}

qNaN :: RealFloat a => Quaternion a
qNaN = Quaternion fNaN (V3 fNaN fNaN fNaN) where fNaN = 0/0
{-# INLINE qNaN #-}

-- {-# RULES "abs/norm" abs x = Quaternion (norm x) 0 #-}
-- {-# RULES "signum/signorm" signum = signorm #-}

-- this will attempt to rewrite calls to abs to use norm intead when it is available.

instance RealFloat a => Fractional (Quaternion a) where
  {-# SPECIALIZE instance Fractional (Quaternion Float) #-}
  {-# SPECIALIZE instance Fractional (Quaternion Double) #-}
  Quaternion q0 (V3 q1 q2 q3) / Quaternion r0 (V3 r1 r2 r3) =
    Quaternion (r0*q0+r1*q1+r2*q2+r3*q3)
               (V3 (r0*q1-r1*q0-r2*q3+r3*q2)
                   (r0*q2+r1*q3-r2*q0-r3*q1)
                   (r0*q3-r1*q2+r2*q1-r3*q0))
               ^/ (r0*r0 + r1*r1 + r2*r2 + r3*r3)
  {-# INLINE (/) #-}
  recip q = q ^/ quadrance q
  {-# INLINE recip #-}
  fromRational x = Quaternion (fromRational x) 0
  {-# INLINE fromRational #-}

instance Metric Quaternion where
  Quaternion e v `dot` Quaternion e' v' = e*e' + (v `dot` v')
  {-# INLINE dot #-}

-- | A vector space that includes the basis elements '_e' and '_i'
class Complicated t where
  _e, _i :: Lens' (t a) a

ee, ei :: Complicated t => E t
ee = E _e
ei = E _i

instance Complicated Complex where
  _e f (a :+ b) = (:+ b) <$> f a
  {-# INLINE _e #-}
  _i f (a :+ b) = (a :+) <$> f b
  {-# INLINE _i #-}

instance Complicated Quaternion where
  _e f (Quaternion a v) = (`Quaternion` v) <$> f a
  {-# INLINE _e #-}
  _i f (Quaternion a v) = Quaternion a <$> _x f v
  {-# INLINE _i #-}

-- | A vector space that includes the basis elements '_e', '_i', '_j' and '_k'
class Complicated t => Hamiltonian t where
  _j, _k :: Lens' (t a) a
  _ijk :: Lens' (t a) (V3 a)

ej, ek :: Hamiltonian t => E t
ej = E _j
ek = E _k

instance Hamiltonian Quaternion where
  _j f (Quaternion a v) = Quaternion a <$> _y f v
  {-# INLINE _j #-}
  _k f (Quaternion a v) = Quaternion a <$> _z f v
  {-# INLINE _k #-}
  _ijk f (Quaternion a v) = Quaternion a <$> f v
  {-# INLINE _ijk #-}

instance Distributive Quaternion where
  distribute f = Quaternion (fmap (\(Quaternion x _) -> x) f) $ V3
    (fmap (\(Quaternion _ (V3 y _ _)) -> y) f)
    (fmap (\(Quaternion _ (V3 _ z _)) -> z) f)
    (fmap (\(Quaternion _ (V3 _ _ w)) -> w) f)
  {-# INLINE distribute #-}

instance (Conjugate a, RealFloat a) => Conjugate (Quaternion a) where
  conjugate (Quaternion e v) = Quaternion (conjugate e) (negate v)
  {-# INLINE conjugate #-}

reimagine :: RealFloat a => a -> a -> Quaternion a -> Quaternion a
reimagine r s (Quaternion _ v)
  | isNaN s || isInfinite s = let aux 0 = 0
                                  aux x = s * x
                              in Quaternion r (aux <$> v)
  | otherwise = Quaternion r (v^*s)
{-# INLINE reimagine #-}

-- | quadrance of the imaginary component
qi :: Num a => Quaternion a -> a
qi (Quaternion _ v) = quadrance v
{-# INLINE qi #-}

-- | norm of the imaginary component
absi :: Floating a => Quaternion a -> a
absi = sqrt . qi
{-# INLINE absi #-}

-- | raise a 'Quaternion' to a scalar power
pow :: RealFloat a => Quaternion a -> a -> Quaternion a
pow q t = exp (t *^ log q)
{-# INLINE pow #-}

-- ehh..
instance RealFloat a => Floating (Quaternion a) where
  {-# SPECIALIZE instance Floating (Quaternion Float) #-}
  {-# SPECIALIZE instance Floating (Quaternion Double) #-}
  pi = Quaternion pi 0
  {-# INLINE pi #-}
  exp q@(Quaternion e v)
    | qiq == 0 = Quaternion (exp e) v
    | ai <- sqrt qiq, exe <- exp e = reimagine (exe * cos ai) (exe * (sin ai / ai)) q
    where qiq = qi q
  {-# INLINE exp #-}
  log q@(Quaternion e v@(V3 _i j k))
    | qiq == 0 = if e >= 0
                 then Quaternion (log e) v
                 else Quaternion (log (negate e)) (V3 pi j k) -- mmm, pi
    | ai <- sqrt qiq, m <- sqrt (e*e + qiq) = reimagine (log m) (atan2 m e / ai) q
    where qiq = qi q
  {-# INLINE log #-}
  x ** y = exp (y * log x)
  {-# INLINE (**) #-}
  sqrt q@(Quaternion e v)
    | m   == 0 = q
    | qiq == 0 = if e > 0
                 then Quaternion (sqrt e) 0
                 else Quaternion 0 (V3 (sqrt (negate e)) 0 0)
    | im <- sqrt (0.5*(m-e)) / sqrt qiq = Quaternion (0.5*(m+e)) (v^*im)
    where qiq = qi q
          m = sqrt (e*e + qiq)
  {-# INLINE sqrt #-}
  cos q@(Quaternion e v)
    | qiq == 0 = Quaternion (cos e) v
    | ai <- sqrt qiq = reimagine (cos e * cosh ai) (- sin e * (sinh ai / ai)) q
    where qiq = qi q
  {-# INLINE cos #-}
  sin q@(Quaternion e v)
    | qiq == 0 = Quaternion (sin e) v
    | ai <- sqrt qiq = reimagine (sin e * cosh ai) (cos e * (sinh ai / ai)) q
    where qiq = qi q
  {-# INLINE sin #-}
  tan q@(Quaternion e v)
    | qiq == 0 = Quaternion (tan e) v
    | ai <- sqrt qiq, ce <- cos e, sai <- sinh ai, d <- ce*ce + sai*sai =
      reimagine (ce * sin e / d) (cosh ai * (sai / ai) / d) q
    where qiq = qi q
  {-# INLINE tan #-}
  sinh q@(Quaternion e v)
    | qiq == 0 = Quaternion (sinh e) v
    | ai <- sqrt qiq = reimagine (sinh e * cos ai) (cosh e * (sin ai / ai)) q
    where qiq = qi q
  {-# INLINE sinh #-}
  cosh q@(Quaternion e v)
    | qiq == 0 = Quaternion (cosh e) v
    | ai <- sqrt qiq = reimagine (cosh e * cos ai) ((sinh e * sin ai) / ai) q
    where qiq = qi q
  {-# INLINE cosh #-}
  tanh q@(Quaternion e v)
    | qiq == 0 = Quaternion (tanh e) v
    | ai <- sqrt qiq, se <- sinh e, cai <- cos ai, d <- se*se + cai*cai =
      reimagine ((cosh e * se) / d) ((cai * (sin ai / ai)) / d) q
    where qiq = qi q
  {-# INLINE tanh #-}

  asin = cut asin
  {-# INLINE asin #-}
  acos = cut acos
  {-# INLINE acos #-}
  atan = cut atan
  {-# INLINE atan #-}

  asinh = cut asinh
  {-# INLINE asinh #-}
  acosh = cut acosh
  {-# INLINE acosh #-}
  atanh = cut atanh
  {-# INLINE atanh #-}


-- | Helper for calculating with specific branch cuts
cut :: RealFloat a => (Complex a -> Complex a) -> Quaternion a -> Quaternion a
cut f q@(Quaternion e (V3 _ y z))
  | qiq == 0 = Quaternion a (V3 b y z)
  | otherwise = reimagine a (b / ai) q
  where qiq = qi q
        ai = sqrt qiq
        a :+ b = f (e :+ ai)
{-# INLINE cut #-}

-- | Helper for calculating with specific branch cuts
cutWith :: RealFloat a => Complex a -> Quaternion a -> Quaternion a
cutWith (r :+ im) q@(Quaternion e v)
  | e /= 0 || qiq == 0 || isNaN qiq || isInfinite qiq = error "bad cut"
  | s <- im / sqrt qiq = Quaternion r (v^*s)
  where qiq = qi q
{-# INLINE cutWith #-}

-- | 'asin' with a specified branch cut.
asinq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
asinq q@(Quaternion e _) u
  | qiq /= 0.0 || e >= -1 && e <= 1 = asin q
  | otherwise = cutWith (asin (e :+ sqrt qiq)) u
  where qiq = qi q
{-# INLINE asinq #-}

-- | 'acos' with a specified branch cut.
acosq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
acosq q@(Quaternion e _) u
  | qiq /= 0.0 || e >= -1 && e <= 1 = acos q
  | otherwise = cutWith (acos (e :+ sqrt qiq)) u
  where qiq = qi q
{-# INLINE acosq #-}

-- | 'atan' with a specified branch cut.
atanq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
atanq q@(Quaternion e _) u
  | e /= 0.0 || qiq >= -1 && qiq <= 1 = atan q
  | otherwise = cutWith (atan (e :+ sqrt qiq)) u
  where qiq = qi q
{-# INLINE atanq #-}

-- | 'asinh' with a specified branch cut.
asinhq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
asinhq q@(Quaternion e _) u
  | e /= 0.0 || qiq >= -1 && qiq <= 1 = asinh q
  | otherwise = cutWith (asinh (e :+ sqrt qiq)) u
  where qiq = qi q
{-# INLINE asinhq #-}

-- | 'acosh' with a specified branch cut.
acoshq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
acoshq q@(Quaternion e _) u
  | qiq /= 0.0 || e >= 1 = asinh q
  | otherwise = cutWith (acosh (e :+ sqrt qiq)) u
  where qiq = qi q
{-# INLINE acoshq #-}

-- | 'atanh' with a specified branch cut.
atanhq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
atanhq q@(Quaternion e _) u
  | qiq /= 0.0 || e > -1 && e < 1 = atanh q
  | otherwise = cutWith (atanh (e :+ sqrt qiq)) u
  where qiq = qi q
{-# INLINE atanhq #-}

-- | Spherical linear interpolation between two quaternions.

slerp :: RealFloat a => Quaternion a -> Quaternion a -> a -> Quaternion a
slerp q p t
  | 1.0 - cosphi < 1e-8 = q
  | otherwise           = ((sin ((1-t)*phi) *^ q) + sin (t*phi) *^ f p) ^/ sin phi
  where
    dqp = dot q p
    (cosphi, f) = if dqp < 0 then (-dqp, negate) else (dqp, id)
    phi = acos cosphi
{-# SPECIALIZE slerp :: Quaternion Float -> Quaternion Float -> Float -> Quaternion Float #-}
{-# SPECIALIZE slerp :: Quaternion Double -> Quaternion Double -> Double -> Quaternion Double #-}

-- | Apply a rotation to a vector.
rotate :: (Conjugate a, RealFloat a) => Quaternion a -> V3 a -> V3 a
rotate q v = ijk where
  Quaternion _ ijk = q * Quaternion 0 v * conjugate q
{-# SPECIALIZE rotate :: Quaternion Float -> V3 Float -> V3 Float #-}
{-# SPECIALIZE rotate :: Quaternion Double -> V3 Double -> V3 Double #-}

instance (RealFloat a, Epsilon a) => Epsilon (Quaternion a) where
  nearZero = nearZero . quadrance
  {-# INLINE nearZero #-}

-- | @'axisAngle' axis theta@ builds a 'Quaternion' representing a
-- rotation of @theta@ radians about @axis@.
axisAngle :: (Epsilon a, Floating a) => V3 a -> a -> Quaternion a
axisAngle axis theta = Quaternion (cos half) (sin half *^ normalize axis)
  where half = theta / 2
{-# INLINE axisAngle #-}

data instance U.Vector    (Quaternion a) =  V_Quaternion !Int (U.Vector    a)
data instance U.MVector s (Quaternion a) = MV_Quaternion !Int (U.MVector s a)
instance U.Unbox a => U.Unbox (Quaternion a)

instance U.Unbox a => M.MVector U.MVector (Quaternion a) where
  basicLength (MV_Quaternion n _) = n
  basicUnsafeSlice m n (MV_Quaternion _ v) = MV_Quaternion n (M.basicUnsafeSlice (4*m) (4*n) v)
  basicOverlaps (MV_Quaternion _ v) (MV_Quaternion _ u) = M.basicOverlaps v u
  basicUnsafeNew n = liftM (MV_Quaternion n) (M.basicUnsafeNew (4*n))
  basicUnsafeRead (MV_Quaternion _ v) i =
    do let o = 4*i
       x <- M.basicUnsafeRead v o
       y <- M.basicUnsafeRead v (o+1)
       z <- M.basicUnsafeRead v (o+2)
       w <- M.basicUnsafeRead v (o+3)
       return (Quaternion x (V3 y z w))
  basicUnsafeWrite (MV_Quaternion _ v) i (Quaternion x (V3 y z w)) =
    do let o = 4*i
       M.basicUnsafeWrite v o     x
       M.basicUnsafeWrite v (o+1) y
       M.basicUnsafeWrite v (o+2) z
       M.basicUnsafeWrite v (o+3) w

instance U.Unbox a => G.Vector U.Vector (Quaternion a) where
  basicUnsafeFreeze (MV_Quaternion n v) = liftM ( V_Quaternion n) (G.basicUnsafeFreeze v)
  basicUnsafeThaw   ( V_Quaternion n v) = liftM (MV_Quaternion n) (G.basicUnsafeThaw   v)
  basicLength       ( V_Quaternion n _) = n
  basicUnsafeSlice m n (V_Quaternion _ v) = V_Quaternion n (G.basicUnsafeSlice (4*m) (4*n) v)
  basicUnsafeIndexM (V_Quaternion _ v) i =
    do let o = 4*i
       x <- G.basicUnsafeIndexM v o
       y <- G.basicUnsafeIndexM v (o+1)
       z <- G.basicUnsafeIndexM v (o+2)
       w <- G.basicUnsafeIndexM v (o+3)
       return (Quaternion x (V3 y z w))

instance MonadZip Quaternion where
  mzipWith = liftA2

instance MonadFix Quaternion where
  mfix f = Quaternion (let Quaternion a _ = f a in a)
                      (V3 (let Quaternion _ (V3 a _ _) = f a in a)
                          (let Quaternion _ (V3 _ a _) = f a in a)
                          (let Quaternion _ (V3 _ _ a) = f a in a))

instance NFData a => NFData (Quaternion a) where
  rnf (Quaternion a b) = rnf a `seq` rnf b

instance Serial a => Serial (Quaternion a)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
instance Serial1 Quaternion

instance Binary a => Binary (Quaternion a) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance Serialize a => Serialize (Quaternion a) where
  put = serializeWith Cereal.put
  get = deserializeWith Cereal.get
#endif

instance Eq1 Quaternion where eq1 = (==)
instance Ord1 Quaternion where compare1 = compare
instance Show1 Quaternion where showsPrec1 = showsPrec
instance Read1 Quaternion where readsPrec1 = readsPrec

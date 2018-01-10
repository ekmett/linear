{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
#endif
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DataKinds #-}
#endif

#ifndef MIN_VERSION_vector
#define MIN_VERSION_vector(x,y,z) 1
#endif

#ifndef MIN_VERSION_transformers
#define MIN_VERSION_transformers(x,y,z) 1
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
-- Plücker coordinates for lines in 3d homogeneous space.
----------------------------------------------------------------------------
module Linear.Plucker
  ( Plucker(..)
  , squaredError
  , isotropic
  , (><)
  , plucker
  , plucker3D
  -- * Operations on lines
  , parallel
  , intersects
  , LinePass(..)
  , passes
  , quadranceToOrigin
  , closestToOrigin
  , isLine
  , coincides
  , coincides'
  -- * Basis elements
  ,      p01, p02, p03
  , p10,      p12, p13
  , p20, p21,      p23
  , p30, p31, p32

  , e01, e02, e03, e12, e31, e23
  ) where

import Control.Applicative
import Control.DeepSeq (NFData(rnf))
import Control.Monad (liftM)
import Control.Monad.Fix
import Control.Monad.Zip
import Control.Lens hiding (index, (<.>))
import Data.Binary as Binary
import Data.Bytes.Serial
import Data.Distributive
import Data.Foldable as Foldable
import Data.Functor.Bind
import Data.Functor.Classes
import Data.Functor.Rep
import Data.Hashable
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Serialize as Cereal
#if __GLASGOW_HASKELL__ >= 707
import qualified Data.Vector as V
#endif
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import GHC.Arr (Ix(..))
#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif
#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
#endif
import Linear.Epsilon
import Linear.Metric
#if __GLASGOW_HASKELL__ >= 707
import Linear.V
#endif
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector

{-# ANN module "HLint: ignore Reduce duplication" #-}

-- | Plücker coordinates for lines in a 3-dimensional space.
data Plucker a = Plucker !a !a !a !a !a !a deriving (Eq,Ord,Show,Read
#if __GLASGOW_HASKELL__ >= 702
                                                    ,Generic
#endif
#if __GLASGOW_HASKELL__ >= 706
                                                    ,Generic1
#endif
                                                    )

#if __GLASGOW_HASKELL__ >= 707
instance Finite Plucker where
  type Size Plucker = 6
  toV (Plucker a b c d e f) = V (V.fromListN 6 [a,b,c,d,e,f])
  fromV (V v) = Plucker (v V.! 0) (v V.! 1) (v V.! 2) (v V.! 3) (v V.! 4) (v V.! 5)
#endif

instance Functor Plucker where
  fmap g (Plucker a b c d e f) = Plucker (g a) (g b) (g c) (g d) (g e) (g f)
  {-# INLINE fmap #-}

instance Apply Plucker where
  Plucker a b c d e f <.> Plucker g h i j k l =
    Plucker (a g) (b h) (c i) (d j) (e k) (f l)
  {-# INLINE (<.>) #-}

instance Applicative Plucker where
  pure a = Plucker a a a a a a
  {-# INLINE pure #-}
  Plucker a b c d e f <*> Plucker g h i j k l =
    Plucker (a g) (b h) (c i) (d j) (e k) (f l)
  {-# INLINE (<*>) #-}

instance Additive Plucker where
  zero = pure 0
  {-# INLINE zero #-}
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

instance Bind Plucker where
  Plucker a b c d e f >>- g = Plucker a' b' c' d' e' f' where
    Plucker a' _ _ _ _ _ = g a
    Plucker _ b' _ _ _ _ = g b
    Plucker _ _ c' _ _ _ = g c
    Plucker _ _ _ d' _ _ = g d
    Plucker _ _ _ _ e' _ = g e
    Plucker _ _ _ _ _ f' = g f
  {-# INLINE (>>-) #-}

instance Monad Plucker where
  return a = Plucker a a a a a a
  {-# INLINE return #-}
  Plucker a b c d e f >>= g = Plucker a' b' c' d' e' f' where
    Plucker a' _ _ _ _ _ = g a
    Plucker _ b' _ _ _ _ = g b
    Plucker _ _ c' _ _ _ = g c
    Plucker _ _ _ d' _ _ = g d
    Plucker _ _ _ _ e' _ = g e
    Plucker _ _ _ _ _ f' = g f
  {-# INLINE (>>=) #-}

instance Distributive Plucker where
  distribute f = Plucker (fmap (\(Plucker x _ _ _ _ _) -> x) f)
                         (fmap (\(Plucker _ x _ _ _ _) -> x) f)
                         (fmap (\(Plucker _ _ x _ _ _) -> x) f)
                         (fmap (\(Plucker _ _ _ x _ _) -> x) f)
                         (fmap (\(Plucker _ _ _ _ x _) -> x) f)
                         (fmap (\(Plucker _ _ _ _ _ x) -> x) f)
  {-# INLINE distribute #-}

instance Representable Plucker where
  type Rep Plucker = E Plucker
  tabulate f = Plucker (f e01) (f e02) (f e03) (f e23) (f e31) (f e12)
  {-# INLINE tabulate #-}
  index xs (E l) = view l xs
  {-# INLINE index #-}

instance Foldable Plucker where
  foldMap g (Plucker a b c d e f) =
    g a `mappend` g b `mappend` g c `mappend` g d `mappend` g e `mappend` g f
  {-# INLINE foldMap #-}
#if __GLASGOW_HASKELL__ >= 710
  null _ = False
  length _ =  6
#endif

instance Traversable Plucker where
  traverse g (Plucker a b c d e f) =
    Plucker <$> g a <*> g b <*> g c <*> g d <*> g e <*> g f
  {-# INLINE traverse #-}

instance Foldable1 Plucker where
  foldMap1 g (Plucker a b c d e f) =
    g a <> g b <> g c <> g d <> g e <> g f
  {-# INLINE foldMap1 #-}

instance Traversable1 Plucker where
  traverse1 g (Plucker a b c d e f) =
    Plucker <$> g a <.> g b <.> g c <.> g d <.> g e <.> g f
  {-# INLINE traverse1 #-}

instance Ix a => Ix (Plucker a) where
  range (Plucker l1 l2 l3 l4 l5 l6,Plucker u1 u2 u3 u4 u5 u6) =
    [Plucker i1 i2 i3 i4 i5 i6 | i1 <- range (l1,u1)
                     , i2 <- range (l2,u2)
                     , i3 <- range (l3,u3)
                     , i4 <- range (l4,u4)
                     , i5 <- range (l5,u5)
                     , i6 <- range (l6,u6)
                     ]
  {-# INLINE range #-}

  unsafeIndex (Plucker l1 l2 l3 l4 l5 l6,Plucker u1 u2 u3 u4 u5 u6) (Plucker i1 i2 i3 i4 i5 i6) =
    unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
    unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
    unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
    unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
    unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) *
    unsafeIndex (l1,u1) i1))))
  {-# INLINE unsafeIndex #-}

  inRange (Plucker l1 l2 l3 l4 l5 l6,Plucker u1 u2 u3 u4 u5 u6) (Plucker i1 i2 i3 i4 i5 i6) =
    inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
    inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
    inRange (l5,u5) i5 && inRange (l6,u6) i6
  {-# INLINE inRange #-}

instance Num a => Num (Plucker a) where
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

instance Fractional a => Fractional (Plucker a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Floating a => Floating (Plucker a) where
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

instance Hashable a => Hashable (Plucker a) where
  hashWithSalt s (Plucker a b c d e f) = s `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f
  {-# INLINE hashWithSalt #-}

instance Storable a => Storable (Plucker a) where
  sizeOf _ = 6 * sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (Plucker a b c d e f) = do
    poke ptr' a
    pokeElemOff ptr' 1 b
    pokeElemOff ptr' 2 c
    pokeElemOff ptr' 3 d
    pokeElemOff ptr' 4 e
    pokeElemOff ptr' 5 f
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = Plucker <$> peek ptr'
                     <*> peekElemOff ptr' 1
                     <*> peekElemOff ptr' 2
                     <*> peekElemOff ptr' 3
                     <*> peekElemOff ptr' 4
                     <*> peekElemOff ptr' 5
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Metric Plucker where
  dot (Plucker a b c d e f) (Plucker g h i j k l) = a*g+b*h+c*i+d*j+e*k+f*l
  {-# INLINE dot #-}

instance Epsilon a => Epsilon (Plucker a) where
  nearZero = nearZero . quadrance
  {-# INLINE nearZero #-}

-- | Given a pair of points represented by homogeneous coordinates
-- generate Plücker coordinates for the line through them, directed
-- from the second towards the first.
plucker :: Num a => V4 a -> V4 a -> Plucker a
plucker (V4 a b c d)
        (V4 e f g h) =
  Plucker (a*f-b*e)
          (a*g-c*e)
          (b*g-c*f)
          (a*h-d*e)
          (b*h-d*f)
          (c*h-d*g)
{-# INLINE plucker #-}

-- | Given a pair of 3D points, generate Plücker coordinates for the
-- line through them, directed from the second towards the first.
plucker3D :: Num a => V3 a -> V3 a -> Plucker a
plucker3D p q = Plucker a b c d e f
  where V3 a b c = p - q
        V3 d e f = p `cross` q

-- | These elements form a basis for the Plücker space, or the Grassmanian manifold @Gr(2,V4)@.
--
-- @
-- 'p01' :: 'Lens'' ('Plucker' a) a
-- 'p02' :: 'Lens'' ('Plucker' a) a
-- 'p03' :: 'Lens'' ('Plucker' a) a
-- 'p23' :: 'Lens'' ('Plucker' a) a
-- 'p31' :: 'Lens'' ('Plucker' a) a
-- 'p12' :: 'Lens'' ('Plucker' a) a
-- @
p01, p02, p03, p23, p31, p12 :: Lens' (Plucker a) a
p01 g (Plucker a b c d e f) = (\a' -> Plucker a' b c d e f) <$> g a
p02 g (Plucker a b c d e f) = (\b' -> Plucker a b' c d e f) <$> g b
p03 g (Plucker a b c d e f) = (\c' -> Plucker a b c' d e f) <$> g c
p23 g (Plucker a b c d e f) = (\d' -> Plucker a b c d' e f) <$> g d
p31 g (Plucker a b c d e f) = (\e' -> Plucker a b c d e' f) <$> g e
p12 g (Plucker a b c d e f) = Plucker a b c d e <$> g f
{-# INLINE p01 #-}
{-# INLINE p02 #-}
{-# INLINE p03 #-}
{-# INLINE p23 #-}
{-# INLINE p31 #-}
{-# INLINE p12 #-}

-- | These elements form an alternate basis for the Plücker space, or the Grassmanian manifold @Gr(2,V4)@.
--
-- @
-- 'p10' :: 'Num' a => 'Lens'' ('Plucker' a) a
-- 'p20' :: 'Num' a => 'Lens'' ('Plucker' a) a
-- 'p30' :: 'Num' a => 'Lens'' ('Plucker' a) a
-- 'p32' :: 'Num' a => 'Lens'' ('Plucker' a) a
-- 'p13' :: 'Num' a => 'Lens'' ('Plucker' a) a
-- 'p21' :: 'Num' a => 'Lens'' ('Plucker' a) a
-- @
p10, p20, p30, p32, p13, p21 :: (Functor f, Num a) => (a -> f a) -> Plucker a -> f (Plucker a)
p10 = anti p01
p20 = anti p02
p30 = anti p03
p32 = anti p23
p13 = anti p31
p21 = anti p21
{-# INLINE p10 #-}
{-# INLINE p20 #-}
{-# INLINE p30 #-}
{-# INLINE p32 #-}
{-# INLINE p13 #-}
{-# INLINE p21 #-}

anti :: (Functor f, Num a) => ((a -> f a) -> r) -> (a -> f a) -> r
anti k f = k (fmap negate . f . negate)

e01, e02, e03, e23, e31, e12 :: E Plucker
e01 = E p01
e02 = E p02
e03 = E p03
e23 = E p23
e31 = E p31
e12 = E p12

instance FunctorWithIndex (E Plucker) Plucker where
  imap f (Plucker a b c d e g) = Plucker (f e01 a) (f e02 b) (f e03 c) (f e23 d) (f e31 e) (f e12 g)
  {-# INLINE imap #-}

instance FoldableWithIndex (E Plucker) Plucker where
  ifoldMap f (Plucker a b c d e g) = f e01 a `mappend` f e02 b `mappend` f e03 c
                           `mappend` f e23 d `mappend` f e31 e `mappend` f e12 g
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex (E Plucker) Plucker where
  itraverse f (Plucker a b c d e g) = Plucker <$> f e01 a <*> f e02 b <*> f e03 c
                                              <*> f e23 d <*> f e31 e <*> f e12 g
  {-# INLINE itraverse #-}

type instance Index (Plucker a) = E Plucker
type instance IxValue (Plucker a) = a

instance Ixed (Plucker a) where
  ix = el
  {-# INLINE ix #-}

instance Each (Plucker a) (Plucker b) a b where
  each = traverse
  {-# INLINE each #-}


-- | Valid Plücker coordinates @p@ will have @'squaredError' p '==' 0@
--
-- That said, floating point makes a mockery of this claim, so you may want to use 'nearZero'.
squaredError :: Num a => Plucker a -> a
squaredError v = v >< v
{-# INLINE squaredError #-}

-- | This isn't th actual metric because this bilinear form gives rise to an isotropic quadratic space
infixl 5 ><
(><) :: Num a => Plucker a -> Plucker a -> a
Plucker a b c d e f >< Plucker g h i j k l = a*l-b*k+c*j+d*i-e*h+f*g
{-# INLINE (><) #-}

-- | Checks if the line is near-isotropic (isotropic vectors in this
-- quadratic space represent lines in real 3d space).
isotropic :: Epsilon a => Plucker a -> Bool
isotropic a = nearZero (a >< a)
{-# INLINE isotropic #-}

-- | Checks if two lines intersect (or nearly intersect).
intersects :: (Epsilon a, Ord a) => Plucker a -> Plucker a -> Bool
intersects a b = not (a `parallel` b) && passes a b == Coplanar
-- intersects :: Epsilon a => Plucker a -> Plucker a -> Bool
-- intersects a b = nearZero (a >< b)
{-# INLINE intersects #-}

-- | Describe how two lines pass each other.
data LinePass = Coplanar
              -- ^ The lines are coplanar (parallel or intersecting).
              | Clockwise
              -- ^ The lines pass each other clockwise (right-handed
              -- screw)
              | Counterclockwise
              -- ^ The lines pass each other counterclockwise
              -- (left-handed screw).
                deriving (Eq, Show
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
                         ,Generic
#endif
                         )

-- | Check how two lines pass each other. @passes l1 l2@ describes
-- @l2@ when looking down @l1@.
passes :: (Epsilon a, Ord a) => Plucker a -> Plucker a -> LinePass
passes a b
  | nearZero s = Coplanar
  | s > 0 = Counterclockwise
  | otherwise = Clockwise
  where s = (u1 `dot` v2) + (u2 `dot` v1)
        V2 u1 v1 = toUV a
        V2 u2 v2 = toUV b
{-# INLINE passes #-}

-- | Checks if two lines are parallel.
parallel :: Epsilon a => Plucker a -> Plucker a -> Bool
parallel a b = nearZero $ u1 `cross` u2
  where V2 u1 _ = toUV a
        V2 u2 _ = toUV b
{-# INLINE parallel #-}

-- | Represent a Plücker coordinate as a pair of 3-tuples, typically
-- denoted U and V.
toUV :: Plucker a -> V2 (V3 a)
toUV (Plucker a b c d e f) = V2 (V3 a b c) (V3 d e f)

-- | Checks if two lines coincide in space. In other words, undirected equality.
coincides :: (Epsilon a, Fractional a) => Plucker a -> Plucker a -> Bool
coincides p1 p2 = Foldable.all nearZero $ (s *^ p2) - p1
  where s = maybe 1 getFirst . getOption . fold $ saveDiv <$> p1 <*> p2
        saveDiv x y | nearZero y = Option Nothing
                    | otherwise  = Option . Just $ First (x / y)
{-# INLINABLE coincides #-}

-- | Checks if two lines coincide in space, and have the same
-- orientation.
coincides' :: (Epsilon a, Fractional a, Ord a) => Plucker a -> Plucker a -> Bool
coincides' p1 p2 = Foldable.all nearZero ((s *^ p2) - p1) && s > 0
  where s = maybe 1 getFirst . getOption . fold $ saveDiv <$> p1 <*> p2
        saveDiv x y | nearZero y = Option Nothing
                    | otherwise  = Option . Just $ First (x / y)
{-# INLINABLE coincides' #-}

-- | The minimum squared distance of a line from the origin.
quadranceToOrigin :: Fractional a => Plucker a -> a
quadranceToOrigin p = (v `dot` v) / (u `dot` u)
  where V2 u v = toUV p
{-# INLINE quadranceToOrigin #-}

-- | The point where a line is closest to the origin.
closestToOrigin :: Fractional a => Plucker a -> V3 a
closestToOrigin p = normalizePoint $ V4 x y z (u `dot` u)
  where V2 u v = toUV p
        V3 x y z = v `cross` u
{-# INLINE closestToOrigin #-}

-- | Not all 6-dimensional points correspond to a line in 3D. This
-- predicate tests that a Plücker coordinate lies on the Grassmann
-- manifold, and does indeed represent a 3D line.
isLine :: Epsilon a => Plucker a -> Bool
isLine p = nearZero $ u `dot` v
  where V2 u v = toUV p
{-# INLINE isLine #-}

-- TODO: drag some more stuff out of my thesis

data instance U.Vector    (Plucker a) =  V_Plucker !Int (U.Vector    a)
data instance U.MVector s (Plucker a) = MV_Plucker !Int (U.MVector s a)
instance U.Unbox a => U.Unbox (Plucker a)

instance U.Unbox a => M.MVector U.MVector (Plucker a) where
  basicLength (MV_Plucker n _) = n
  basicUnsafeSlice m n (MV_Plucker _ v) = MV_Plucker n (M.basicUnsafeSlice (6*m) (6*n) v)
  basicOverlaps (MV_Plucker _ v) (MV_Plucker _ u) = M.basicOverlaps v u
  basicUnsafeNew n = liftM (MV_Plucker n) (M.basicUnsafeNew (6*n))
  basicUnsafeRead (MV_Plucker _ a) i =
    do let o = 6*i
       x <- M.basicUnsafeRead a o
       y <- M.basicUnsafeRead a (o+1)
       z <- M.basicUnsafeRead a (o+2)
       w <- M.basicUnsafeRead a (o+3)
       v <- M.basicUnsafeRead a (o+4)
       u <- M.basicUnsafeRead a (o+5)
       return (Plucker x y z w v u)
  basicUnsafeWrite (MV_Plucker _ a) i (Plucker x y z w v u) =
    do let o = 6*i
       M.basicUnsafeWrite a o     x
       M.basicUnsafeWrite a (o+1) y
       M.basicUnsafeWrite a (o+2) z
       M.basicUnsafeWrite a (o+3) w
       M.basicUnsafeWrite a (o+4) v
       M.basicUnsafeWrite a (o+5) u
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Plucker _ v) = M.basicInitialize v
#endif

instance U.Unbox a => G.Vector U.Vector (Plucker a) where
  basicUnsafeFreeze (MV_Plucker n v) = liftM ( V_Plucker n) (G.basicUnsafeFreeze v)
  basicUnsafeThaw   ( V_Plucker n v) = liftM (MV_Plucker n) (G.basicUnsafeThaw   v)
  basicLength       ( V_Plucker n _) = n
  basicUnsafeSlice m n (V_Plucker _ v) = V_Plucker n (G.basicUnsafeSlice (6*m) (6*n) v)
  basicUnsafeIndexM (V_Plucker _ a) i =
    do let o = 6*i
       x <- G.basicUnsafeIndexM a o
       y <- G.basicUnsafeIndexM a (o+1)
       z <- G.basicUnsafeIndexM a (o+2)
       w <- G.basicUnsafeIndexM a (o+3)
       v <- G.basicUnsafeIndexM a (o+4)
       u <- G.basicUnsafeIndexM a (o+5)
       return (Plucker x y z w v u)

instance MonadZip Plucker where
  mzipWith = liftA2

instance MonadFix Plucker where
  mfix f = Plucker (let Plucker a _ _ _ _ _ = f a in a)
                   (let Plucker _ a _ _ _ _ = f a in a)
                   (let Plucker _ _ a _ _ _ = f a in a)
                   (let Plucker _ _ _ a _ _ = f a in a)
                   (let Plucker _ _ _ _ a _ = f a in a)
                   (let Plucker _ _ _ _ _ a = f a in a)

instance NFData a => NFData (Plucker a) where
  rnf (Plucker a b c d e f) = rnf a `seq` rnf b `seq` rnf c
                        `seq` rnf d `seq` rnf e `seq` rnf f

instance Serial1 Plucker where
  serializeWith = traverse_
  deserializeWith k = Plucker <$> k <*> k <*> k <*> k <*> k <*> k

instance Serial a => Serial (Plucker a) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance Binary a => Binary (Plucker a) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance Serialize a => Serialize (Plucker a) where
  put = serializeWith Cereal.put
  get = deserializeWith Cereal.get


#if (MIN_VERSION_transformers(0,5,0)) || !(MIN_VERSION_transformers(0,4,0))
instance Eq1 Plucker where
  liftEq k (Plucker a1 b1 c1 d1 e1 f1)
           (Plucker a2 b2 c2 d2 e2 f2)
         = k a1 a2 && k b1 b2 && k c1 c2 && k d1 d2 && k e1 e2 && k f1 f2
instance Ord1 Plucker where
  liftCompare k (Plucker a1 b1 c1 d1 e1 f1)
                (Plucker a2 b2 c2 d2 e2 f2)
            = k a1 a2 `mappend` k b1 b2 `mappend` k c1 c2 `mappend` k d1 d2 `mappend` k e1 e2 `mappend` k f1 f2
instance Read1 Plucker where
  liftReadsPrec k _ z = readParen (z > 10) $ \r ->
     [ (Plucker a b c d e f, r7)
     | ("Plucker",r1) <- lex r
     , (a,r2) <- k 11 r1
     , (b,r3) <- k 11 r2
     , (c,r4) <- k 11 r3
     , (d,r5) <- k 11 r4
     , (e,r6) <- k 11 r5
     , (f,r7) <- k 11 r6
     ]
instance Show1 Plucker where
  liftShowsPrec k _ z (Plucker a b c d e f) = showParen (z > 10) $
     showString "Plucker " . k 11 a . showChar ' ' . k 11 b . showChar ' ' . k 11 c . showChar ' ' . k 11 d . showChar ' ' . k 11 e . showChar ' ' . k 11 f
#else
instance Eq1 Plucker where eq1 = (==)
instance Ord1 Plucker where compare1 = compare
instance Show1 Plucker where showsPrec1 = showsPrec
instance Read1 Plucker where readsPrec1 = readsPrec
#endif

instance Field1 (Plucker a) (Plucker a) a a where
  _1 f (Plucker x y z u v w) = f x <&> \x' -> Plucker x' y z u v w

instance Field2 (Plucker a) (Plucker a) a a where
  _2 f (Plucker x y z u v w) = f y <&> \y' -> Plucker x y' z u v w

instance Field3 (Plucker a) (Plucker a) a a where
  _3 f (Plucker x y z u v w) = f z <&> \z' -> Plucker x y z' u v w

instance Field4 (Plucker a) (Plucker a) a a where
  _4 f (Plucker x y z u v w) = f u <&> \u' -> Plucker x y z u' v w

instance Field5 (Plucker a) (Plucker a) a a where
  _5 f (Plucker x y z u v w) = f v <&> \v' -> Plucker x y z u v' w

instance Field6 (Plucker a) (Plucker a) a a where
  _6 f (Plucker x y z u v w) = f w <&> \w' -> Plucker x y z u v w'

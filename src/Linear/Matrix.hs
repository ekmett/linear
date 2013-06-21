{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
---------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Simple matrix operation for low-dimensional primitives.
---------------------------------------------------------------------------
module Linear.Matrix
  ( (!*!), (!+!), (!-!), (!*) , (*!), (!!*), (*!!)
  , adjoint
  , M22, M33, M44, M43, m33_to_m44, m43_to_m44
  , det22, det33, inv22, inv33
  , eye2, eye3, eye4
  , Trace(..)
  , translation
  , fromQuaternion
  , mkTransformation
  , mkTransformationMat
  ) where

import Control.Applicative
import Data.Distributive
import Data.Foldable as Foldable
import Linear.Epsilon
import Linear.Metric
import Linear.Quaternion
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Linear.Conjugate
import Linear.Trace

-- $setup
-- >>> import Data.Complex
-- >>> import Data.IntMap
-- >>> import Debug.SimpleReflect.Vars

infixl 7 !*!
-- | Matrix product. This can compute mixed dense-dense, sparse-dense and sparse-sparse matrix products.
--
-- >>> V2 (V3 1 2 3) (V3 4 5 6) !*! V3 (V2 1 2) (V2 3 4) (V2 4 5)
-- V2 (V2 19 25) (V2 43 58)
--
-- >>> V2 (fromList [(1,2)]) (fromList [(2,3)]) !*! fromList [(1,V3 0 0 1), (2, V3 0 0 5)]
-- V2 (V3 0 0 2) (V3 0 0 15)
(!*!) :: (Functor m, Foldable r, Additive r, Distributive n, Num a) => m (r a) -> r (n a) -> m (n a)
f !*! g = fmap (\r -> Foldable.sum . liftI2 (*) r <$> g') f where g' = distribute g

infixl 6 !+!
-- | Entry-wise matrix addition.
--
-- >>> V2 (V3 1 2 3) (V3 4 5 6) !+! V2 (V3 7 8 9) (V3 1 2 3)
-- V2 (V3 8 10 12) (V3 5 7 9)
(!+!) :: (Additive m, Additive n, Num a) => m (n a) -> m (n a) -> m (n a)
as !+! bs = liftU2 (^+^) as bs

infixl 6 !-!
-- | Entry-wise matrix subtraction.
--
-- >>> V2 (V3 1 2 3) (V3 4 5 6) !-! V2 (V3 7 8 9) (V3 1 2 3)
-- V2 (V3 (-6) (-6) (-6)) (V3 3 3 3)
(!-!) :: (Additive m, Additive n, Num a) => m (n a) -> m (n a) -> m (n a)
as !-! bs = liftU2 (^-^) as bs

infixl 7 !*
-- | Matrix * column vector
--
-- >>> V2 (V3 1 2 3) (V3 4 5 6) !* V3 7 8 9
-- V2 50 122
(!*) :: (Functor m, Metric r, Num a) => m (r a) -> r a -> m a
m !* v = dot v <$> m

infixl 7 *!
-- | Row vector * matrix
--
-- >>> V2 1 2 *! V2 (V3 3 4 5) (V3 6 7 8)
-- V3 15 18 21
(*!) :: (Metric r, Distributive n, Num a) => r a -> r (n a) -> n a
f *! g = dot f <$> distribute g

infixl 7 *!!
-- | Scalar-matrix product
--
-- >>> 5 *!! V2 (V2 1 2) (V2 3 4)
-- V2 (V2 5 10) (V2 15 20)
(*!!) :: (Functor m, Functor r, Num a) => a -> m (r a) -> m (r a)
s *!! m = fmap (s *^) m
{-# INLINE (*!!) #-}

infixl 7 !!*
-- | Matrix-scalar product
--
-- >>> V2 (V2 1 2) (V2 3 4) !!* 5
-- V2 (V2 5 10) (V2 15 20)
(!!*) :: (Functor m, Functor r, Num a) => m (r a) -> a -> m (r a)
(!!*) = flip (*!!)
{-# INLINE (!!*) #-}

-- | Hermitian conjugate or conjugate transpose
--
-- >>> adjoint (V2 (V2 (1 :+ 2) (3 :+ 4)) (V2 (5 :+ 6) (7 :+ 8)))
-- V2 (V2 (1.0 :+ (-2.0)) (5.0 :+ (-6.0))) (V2 (3.0 :+ (-4.0)) (7.0 :+ (-8.0)))
adjoint :: (Functor m, Distributive n, Conjugate a) => m (n a) -> n (m a)
adjoint = collect (fmap conjugate)
{-# INLINE adjoint #-}

-- * Matrices
--
-- Matrices use a row-major representation.

-- | A 2x2 matrix with row-major representation
type M22 a = V2 (V2 a)
-- | A 3x3 matrix with row-major representation
type M33 a = V3 (V3 a)
-- | A 4x4 matrix with row-major representation
type M44 a = V4 (V4 a)
-- | A 4x3 matrix with row-major representation
type M43 a = V4 (V3 a)

-- | Build a rotation matrix from a unit 'Quaternion'.
fromQuaternion :: Num a => Quaternion a -> M33 a
fromQuaternion (Quaternion w (V3 x y z)) =
  V3 (V3 (1-2*(y2+z2)) (2*(x*y-z*w)) (2*(x*z+y*w)))
     (V3 (2*(x*y+z*w)) (1-2*(x2+z2)) (2*(y*z-x*w)))
     (V3 (2*(x*z-y*w)) (2*(y*z+x*w)) (1-2*(x2+y2)))
  where x2 = x * x
        y2 = y * y
        z2 = z * z

-- | Build a transformation matrix from a rotation matrix and a
-- translation vector.
mkTransformationMat :: Num a => M33 a -> V3 a -> M44 a
mkTransformationMat (V3 r1 r2 r3) (V3 tx ty tz) =
  V4 (snoc3 r1 tx) (snoc3 r2 ty) (snoc3 r3 tz) (V4 0 0 0 1)
  where snoc3 (V3 x y z) = V4 x y z

-- |Build a transformation matrix from a rotation expressed as a
-- 'Quaternion' and a translation vector.
mkTransformation :: Num a => Quaternion a -> V3 a -> M44 a
mkTransformation = mkTransformationMat . fromQuaternion

-- | Convert from a 4x3 matrix to a 4x4 matrix, extending it with the @[ 0 0 0 1 ]@ column vector
m43_to_m44 :: Num a => M43 a -> M44 a
m43_to_m44
  (V4 (V3 a b c)
      (V3 d e f)
      (V3 g h i)
      (V3 j k l)) =
  V4 (V4 a b c 0)
     (V4 d e f 0)
     (V4 g h i 0)
     (V4 j k l 1)
{-# ANN m43_to_m44 "HLint: ignore Use camelCase" #-}

-- | Convert a 3x3 matrix to a 4x4 matrix extending it with 0's in the new row and column.
m33_to_m44 :: Num a => M33 a -> M44 a
m33_to_m44 (V3 r1 r2 r3) = V4 (vector r1) (vector r2) (vector r3) (point 0)
{-# ANN m33_to_m44 "HLint: ignore Use camelCase" #-}

-- |2x2 identity matrix.
--
-- >>> eye2
-- V2 (V2 1 0) (V2 0 1)
eye2 :: Num a => M22 a
eye2 = V2 (V2 1 0)
          (V2 0 1)

-- |3x3 identity matrix.
--
-- >>> eye3
-- V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
eye3 :: Num a => M33 a
eye3 = V3 (V3 1 0 0)
          (V3 0 1 0)
          (V3 0 0 1)

-- |4x4 identity matrix.
--
-- >>> eye4
-- V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
eye4 :: Num a => M44 a
eye4 = V4 (V4 1 0 0 0)
          (V4 0 1 0 0)
          (V4 0 0 1 0)
          (V4 0 0 0 1)

-- |Extract the translation vector (first three entries of the last
-- column) from a 3x4 or 4x4 matrix
translation :: (R3 t, R4 v, Functor f, Functor t) => (V3 a -> f (V3 a)) -> t (v a) -> f (t a)
translation = (. fmap (^._w)) . _xyz where
  x ^. l = getConst (l Const x)

-- |2x2 matrix determinant.
--
-- >>> det22 (V2 (V2 a b) (V2 c d))
-- a * d - b * c
det22 :: Num a => M22 a -> a
det22 (V2 (V2 a b) (V2 c d)) = a * d - b * c
{-# INLINE det22 #-}

-- |3x3 matrix determinant.
--
-- >>> det33 (V3 (V3 a b c) (V3 d e f) (V3 g h i))
-- a * (e * i - f * h) - d * (b * i - c * h) + g * (b * f - c * e)
det33 :: Num a => M33 a -> a
det33 (V3 (V3 a b c)
          (V3 d e f)
          (V3 g h i)) = a * (e*i-f*h) - d * (b*i-c*h) + g * (b*f-c*e)
{-# INLINE det33 #-}

-- |2x2 matrix inverse.
--
-- >>> inv22 $ V2 (V2 1 2) (V2 3 4)
-- Just (V2 (V2 (-2.0) 1.0) (V2 1.5 (-0.5)))
inv22 :: (Epsilon a, Floating a) => M22 a -> Maybe (M22 a)
inv22 m@(V2 (V2 a b) (V2 c d))
  | nearZero det = Nothing
  | otherwise = Just $ (1 / det) *!! V2 (V2 d (-b)) (V2 (-c) a)
  where det = det22 m
{-# INLINE inv22 #-}

-- |3x3 matrix inverse.
--
-- >>> inv33 $ V3 (V3 1 2 4) (V3 4 2 2) (V3 1 1 1)
-- Just (V3 (V3 0.0 0.5 (-1.0)) (V3 (-0.5) (-0.75) 3.5) (V3 0.5 0.25 (-1.5)))
inv33 :: (Epsilon a, Floating a) => M33 a -> Maybe (M33 a)
inv33 m@(V3 (V3 a b c)
            (V3 d e f)
            (V3 g h i))
  | nearZero det = Nothing
  | otherwise = Just $ (1 / det) *!! V3 (V3 a' b' c')
                                        (V3 d' e' f')
                                        (V3 g' h' i')
  where a' = cofactor (e,f,h,i)
        b' = cofactor (c,b,i,h)
        c' = cofactor (b,c,e,f)
        d' = cofactor (f,d,i,g)
        e' = cofactor (a,c,g,i)
        f' = cofactor (c,a,f,d)
        g' = cofactor (d,e,g,h)
        h' = cofactor (b,a,h,g)
        i' = cofactor (a,b,d,e)
        cofactor (q,r,s,t) = det22 (V2 (V2 q r) (V2 s t))
        det = det33 m
{-# INLINE inv33 #-}

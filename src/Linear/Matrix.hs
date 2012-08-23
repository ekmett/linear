module Linear.Matrix
  ( (!*!), (!*) , (*!)
  , adjoint
  , M33, M44, M43, m33_to_m44, m43_to_m44
  , eye3, eye4
  , trace
  , translation
  , fromQuaternion
  , mkTransformation
  ) where

import Control.Lens
import Data.Distributive
import Data.Functor
import Data.Foldable as Foldable
import Linear.Quaternion
import Linear.V3
import Linear.V4
import Linear.Metric
import Linear.Conjugate

infixl 7 !*!
-- | matrix product
(!*!) :: (Functor m, Metric r, Distributive n, Num a) => m (r a) -> r (n a) -> m (n a)
f !*! g = (\r -> fmap (dot r) g') <$> f
  where g' = distribute g

-- | matrix * column vector
infixl 7 *!
(!*) :: (Functor m, Metric r, Num a) => m (r a) -> r a -> m a
m !* v = dot v <$> m

infixl 7 !*
-- | row vector * matrix
(*!) :: (Metric r, Distributive n, Num a) => r a -> r (n a) -> n a
f *! g = dot f <$> distribute g

-- | hermitian conjugate or conjugate transpose
adjoint :: (Functor m, Distributive n, Conjugate a) => m (n a) -> n (m a)
adjoint = collect (fmap conjugate)
{-# INLINE adjoint #-}

trace :: (Monad f, Foldable f, Num a) => f (f a) -> a
trace m = Foldable.sum (m >>= id)
{-# INLINE trace #-}

-- |Matrices use a row-major representation.
type M33 a = V3 (V3 a)
type M44 a = V4 (V4 a)
type M43 a = V4 (V3 a)

-- |Build a rotation matrix from a 'Quaternion'.
fromQuaternion :: Num a => Quaternion a -> M33 a
fromQuaternion (Quaternion a (V3 b c d)) =
  V3 (V3 (a*a+b*b-c*c-d*d) (2*b*c-2*a*d) (2*b*d+2*a*c))
     (V3 (2*b*c+2*a*d) (a*a-b*b+c*c-d*d) (2*c*d-2*a*b))
     (V3 (2*b*d-2*a*c) (2*c*d+2*a*b) (a*a-b*b-c*c+d*d))

mkTransformationMat :: Num a => M33 a -> V3 a -> M44 a
mkTransformationMat (V3 r1 r2 r3) (V3 tx ty tz) = 
  V4 (snoc3 r1 tx) (snoc3 r2 ty) (snoc3 r3 tz) (_w.~1$0)
  where snoc3 (V3 x y z) w = V4 x y z w

-- |Build a transformation matrix from a rotation expressed as a
-- 'Quaternion' and a translation vector.
mkTransformation :: Num a => Quaternion a -> V3 a -> M44 a
mkTransformation = mkTransformationMat . fromQuaternion

m43_to_m44 :: Num a => M43 a -> M44 a
m43_to_m44
  (V4 (V3 a b c)
      (V3 d e f)
      (V3 g h i)
      (V3 j k l)) =
  (V4 (V4 a b c 0)
      (V4 d e f 0)
      (V4 g h i 0)
      (V4 j k l 1))

m33_to_m44 :: Num a => M33 a -> M44 a
m33_to_m44 (V3 r1 r2 r3) = V4 (vector r1) (vector r2) (vector r3) (point 0)

-- |3x3 identity matrix.
eye3 :: Num a => M33 a
eye3 = V3 (_x.~1$0) (_y.~1$0) (_z.~1$0)

-- |4x4 identity matrix.
eye4 :: Num a => M44 a
eye4 = V4 (_x.~1$0) (_y.~1$0) (_z.~1$0) (_w.~1$0)

-- |Extract the translation vector (first three entries of the last
-- column) from a 3x4 or 4x4 matrix
translation :: (R3 t, R4 v, Functor f, Functor t) => (V3 a -> f (V3 a)) -> t (v a) -> f (t a)
translation = (. fmap (^._w)) . _xyz

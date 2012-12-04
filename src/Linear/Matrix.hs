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

import Control.Applicative
import Control.Lens
import Data.Distributive
import Data.Foldable as Foldable
import Linear.Epsilon
import Linear.Metric
import Linear.Quaternion
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector ((*^))
import Linear.Conjugate

infixl 7 !*!
-- | matrix product
(!*!) :: (Functor m, Foldable r, Applicative r, Distributive n, Num a) => m (r a) -> r (n a) -> m (n a)
f !*! g = fmap (\r -> Foldable.foldr (+) 0 . liftA2 (*) r <$> g') f
  where g' = distribute g

-- | matrix * column vector
infixl 7 *!
(!*) :: (Functor m, Metric r, Num a) => m (r a) -> r a -> m a
m !* v = dot v <$> m

infixl 7 !*
-- | row vector * matrix
(*!) :: (Metric r, Distributive n, Num a) => r a -> r (n a) -> n a
f *! g = dot f <$> distribute g

infixl 7 *!!
-- |Scalar-matrix product.
(*!!) :: (Functor m, Functor r, Num a) => a -> m (r a) -> m (r a)
s *!! m = fmap (s *^) m
{-# INLINE (*!!) #-}

infixl 7 !!*
-- |Matrix-scalar product.
(!!*) :: (Functor m, Functor r, Num a) => m (r a) -> a -> m (r a)
(!!*) = flip (*!!)
{-# INLINE (!!*) #-}


-- | hermitian conjugate or conjugate transpose
adjoint :: (Functor m, Distributive n, Conjugate a) => m (n a) -> n (m a)
adjoint = collect (fmap conjugate)
{-# INLINE adjoint #-}

-- | Compute the trace of a matrix
trace :: (Monad f, Foldable f, Num a) => f (f a) -> a
trace m = Foldable.sum (m >>= id)
{-# INLINE trace #-}

-- | Matrices use a row-major representation.
type M22 a = V2 (V2 a)
type M33 a = V3 (V3 a)
type M44 a = V4 (V4 a)
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

mkTransformationMat :: Num a => M33 a -> V3 a -> M44 a
mkTransformationMat (V3 r1 r2 r3) (V3 tx ty tz) =
  V4 (snoc3 r1 tx) (snoc3 r2 ty) (snoc3 r3 tz) (set _w 1 0)
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
eye3 = V3 (set _x 1 0) (set _y 1 0) (set _z 1 0)

-- |4x4 identity matrix.
eye4 :: Num a => M44 a
eye4 = V4 (set _x 1 0) (set _y 1 0) (set _z 1 0) (set _w 1 0)

-- |Extract the translation vector (first three entries of the last
-- column) from a 3x4 or 4x4 matrix
translation :: (R3 t, R4 v, Functor f, Functor t) => (V3 a -> f (V3 a)) -> t (v a) -> f (t a)
translation = (. fmap (^._w)) . _xyz

-- |2x2 matrix determinant.
det22 (V2 (V2 a b) (V2 c d)) = a * d - b * c
{-# INLINE det22 #-}

-- |3x3 matrix determinant.
det33 (V3 (V3 a b c) 
          (V3 d e f)
          (V3 g h i)) = a * (e*i-f*h) - d * (b*i-c*h) + g * (b*f-c*e)
{-# INLINE det33 #-}

-- |2x2 matrix inverse.
inv22 :: (Epsilon a, Floating a) => M22 a -> Maybe (M22 a)
inv22 m@(V2 (V2 a b) (V2 c d)) 
  | nearZero det = Nothing
  | otherwise = Just $ (1 / det) *!! (V2 (V2 d (-b)) (V2 (-c) a))
  where det = det22 m
{-# INLINE inv22 #-}

-- |3x3 matrix inverse.
inv33 :: (Epsilon a, Floating a) => M33 a -> Maybe (M33 a)
inv33 m@(V3 (V3 a b c)
            (V3 d e f)
            (V3 g h i)) 
  | nearZero det = Nothing
  | otherwise = Just $ (1 / det) *!! (V3 (V3 a' b' c')
                                         (V3 d' e' f')
                                         (V3 g' h' i'))
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

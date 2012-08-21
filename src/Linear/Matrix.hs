module Linear.Matrix
  ( (!*!), (!*) , (*!)
  , adjoint
  , M44, M43, m43_to_m44
  , trace
  , translation
  ) where

import Data.Distributive
import Data.Functor
import Data.Foldable as Foldable
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

type M44 a = V4 (V4 a)
type M43 a = V4 (V3 a)

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

-- extract the translation elements from the last row of a 4x3 or 4x4 matrix
translation :: (R3 v, Functor f) => (V3 a -> f (V3 a)) -> V4 (v a) -> f (V4 (v a))
translation = w.xyz

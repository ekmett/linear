module Linear.Metric
  ( Metric(..)
  ) where

import Control.Applicative

-- free inner product/metric space
class Applicative f => Metric f where
  dot :: Num a => f a -> f a -> a

  quadrance :: Num a => f a -> a
  quadrance v = dot v v

  qd :: Num a => f a -> f a -> a
  qd f g = quadrance (liftA2 (-) f g)

  distance :: Floating a => f a -> f a -> a
  distance f g = norm (liftA2 (-) f g)

  norm :: Floating a => f a -> a
  norm v = sqrt (dot v v)

  signorm :: Floating a => f a -> f a
  signorm v = fmap (/m) v where
    m = norm v

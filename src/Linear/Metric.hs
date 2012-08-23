module Linear.Metric
  ( Metric(..), normalize
  ) where

import Control.Applicative
import Linear.Epsilon

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

-- |Normalize a 'Metric' functor to have unit 'norm'. This function
-- does not change the functor if its 'norm' is 0 or 1.
normalize :: (Floating a, Metric f, Epsilon a) => f a -> f a
normalize v = if nearZero l || nearZero (1-l) then v else fmap (/sqrt l) v
  where l = quadrance v

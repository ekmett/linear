module Linear.Plucker
  ( Plucker(..)
  , squaredError
  , isotropic
  , (><)
  , plucker
  , intersects
  ) where

import Control.Applicative
import Data.Distributive
import Data.Foldable as Foldable
import Data.Monoid
import Data.Traversable
import Linear.Epsilon
import Linear.Metric
import Control.Lens
import Linear.V4

-- | Plücker coordinates for lines in a 3-dimensional space.
data Plucker a = Plucker a a a a a a deriving (Eq,Ord,Show,Read)

instance Functor Plucker where
  fmap g (Plucker a b c d e f) = Plucker (g a) (g b) (g c) (g d) (g e) (g f)

instance Applicative Plucker where
  pure a = Plucker a a a a a a
  Plucker a b c d e f <*> Plucker g h i j k l =
    Plucker (a g) (b h) (c i) (d j) (e k) (f l)

instance Monad Plucker where
  return a = Plucker a a a a a a
  (>>=) = bindRep

instance Distributive Plucker where
  distribute = distributeRep

instance Representable Plucker where
  rep f = Plucker (f p01) (f p02) (f p03) (f p23) (f p31) (f p12)

instance Foldable Plucker where
  foldMap g (Plucker a b c d e f) =
    g a `mappend` g b `mappend` g c `mappend` g d `mappend` g e `mappend` g f

instance Traversable Plucker where
  traverse g (Plucker a b c d e f) =
    Plucker <$> g a <*> g b <*> g c <*> g d <*> g e <*> g f

instance Num a => Num (Plucker a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Plucker a) where
  recip = fmap recip
  (/) = liftA2 (/)
  fromRational = pure . fromRational

-- | Given a pair of points represented by homogeneous coordinates generate Plücker coordinates
-- for the line through them.
plucker :: Num a => V4 a -> V4 a -> Plucker a
plucker (V4 a b c d)
        (V4 e f g h) =
  Plucker (a*f-b*e)
          (a*g-c*e)
          (a*d-h*e)
          (c*h-d*g)
          (d*f-b*h)
          (b*g-c*f)

-- | These elements form a basis for the Plücker space, or the Grassmanian manifold @Gr(2,V4)@.
p01, p02, p03, p23, p31, p12 :: Functor f => (a -> f a) -> Plucker a -> f (Plucker a)
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

-- | Valid Plücker coordinates @p@ will have @'squaredError' p '==' 0@
--
-- That said, floating point makes a mockery of this claim, so you may want to use 'nearZero'.
squaredError :: (Eq a, Num a) => Plucker a -> a
squaredError v = v >< v

-- | This isn't th actual metric because this bilinear form gives rise to an isotropic quadratic space
infixl 5 ><
(><) :: Num a => Plucker a -> Plucker a -> a
Plucker a b c d e f >< Plucker g h i j k l = a*g+b*h+c*i-d*j-e*k-f*l

-- | Checks if the line is near-isotropic (isotropic vectors in this quadratic space represent lines in real 3d space)
isotropic :: Epsilon a => Plucker a -> Bool
isotropic a = nearZero (a >< a)

-- | Checks if the two vectors intersect (or nearly intersect)
intersects :: Epsilon a => Plucker a -> Plucker a -> Bool
intersects a b = nearZero (a >< b)

instance Metric Plucker where
  dot (Plucker a b c d e f) (Plucker g h i j k l) = a*g+b*h+c*i+d*j+e*k+f*l

instance Epsilon a => Epsilon (Plucker a) where
  nearZero = nearZero . quadrance

-- TODO: drag some more stuff out of my thesis

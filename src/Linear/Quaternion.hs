{-# LANGUAGE DeriveDataTypeable, PatternGuards, ScopedTypeVariables #-}
module Linear.Quaternion
  ( Quaternion(..)
  , Complicated(..)
  , Hamiltonian(..)
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
import Control.Lens
import Data.Complex (Complex((:+)))
import Data.Data
import Data.Distributive
import Data.Foldable
import qualified Data.Foldable as F
import Data.Monoid
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Linear.Epsilon
import Linear.Conjugate
import Linear.Metric
import Linear.V3
import Linear.Vector
import Prelude hiding (any)

data Quaternion a = Quaternion a {-# UNPACK #-}!(V3 a)
                    deriving (Eq,Ord,Read,Show,Data,Typeable)

instance Functor Quaternion where
  fmap f (Quaternion e v) = Quaternion (f e) (fmap f v)
  a <$ _ = Quaternion a (V3 a a a)

instance Applicative Quaternion where
  pure a = Quaternion a (pure a)
  Quaternion f fv <*> Quaternion a v = Quaternion (f a) (fv <*> v)

instance Monad Quaternion where
  return = pure
  (>>=) = bindRep -- the diagonal of a sedenion is super useful!

instance Representable Quaternion where
  rep f = Quaternion (f _e) (V3 (f _i) (f _j) (f _k))

instance Foldable Quaternion where
  foldMap f (Quaternion e v) = f e `mappend` foldMap f v
  foldr f z (Quaternion e v) = f e (F.foldr f z v)

instance Traversable Quaternion where
  traverse f (Quaternion e v) = Quaternion <$> f e <*> traverse f v

instance forall a. Storable a => Storable (Quaternion a) where
  sizeOf _ = 4 * sizeOf (undefined::a)
  alignment _ = alignment (undefined::a)
  poke ptr (Quaternion e v) = poke (castPtr ptr) e >>
                              poke (castPtr (ptr `plusPtr` sz)) v
    where sz = sizeOf (undefined::a)
  peek ptr = Quaternion <$> peek (castPtr ptr)
                        <*> peek (castPtr (ptr `plusPtr` sz))
    where sz = sizeOf (undefined::a)

instance RealFloat a => Num (Quaternion a) where
  {-# SPECIALIZE instance Num (Quaternion Float) #-}
  {-# SPECIALIZE instance Num (Quaternion Double) #-}
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  negate = fmap negate
  Quaternion s1 v1 * Quaternion s2 v2 = Quaternion (s1*s2 - (v1 `dot` v2)) $
                                        (v1 `cross` v2) + s1*^v2 + s2*^v1
  fromInteger x = Quaternion (fromInteger x) 0
  abs z = Quaternion (norm z) 0
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

  -- abs    = error "Quaternion.abs: use norm"
  -- signum = error "Quaternion.signum: use signorm"

qNaN :: RealFloat a => Quaternion a
qNaN = Quaternion fNaN (V3 fNaN fNaN fNaN) where fNaN = 0/0

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
  recip q = q ^/ quadrance q
  fromRational x = Quaternion (fromRational x) 0

instance Metric Quaternion where
  Quaternion e v `dot` Quaternion e' v' = e*e' + (v `dot` v')

class Complicated t where
  _e :: Functor f => (a -> f a) -> t a -> f (t a)
  _i :: Functor f => (a -> f a) -> t a -> f (t a)

instance Complicated Complex where
  _e f (a :+ b) = (:+ b) <$> f a
  _i f (a :+ b) = (a :+) <$> f b

instance Complicated Quaternion where
  _e f (Quaternion a v) = (\a' -> Quaternion a' v) <$> f a
  _i f (Quaternion a v) = Quaternion a <$> traverseOf _x f v
  --_i f (Quaternion a (V3 b c d)) = (\b' -> Quaternion a (V3 b' c d)) <$> f b

class Complicated t => Hamiltonian t where
  _j :: Functor f => (a -> f a) -> t a -> f (t a)
  _k :: Functor f => (a -> f a) -> t a -> f (t a)
  _ijk :: Functor f => (V3 a -> f (V3 a)) -> t a -> f (t a)

instance Hamiltonian Quaternion where
  _j f (Quaternion a v) = Quaternion a <$> traverseOf _y f v
  _k f (Quaternion a v) = Quaternion a <$> traverseOf _z f v
  -- _j f (Quaternion a (V3 b c d)) = (\c' -> Quaternion a (V3 b c' d)) <$> f c
  -- _k f (Quaternion a (V3 b c d)) = Quaternion a . V3 b c <$> f d

  _ijk f (Quaternion a v) = Quaternion a <$> f v

instance Distributive Quaternion where
  distribute = distributeRep

instance (Conjugate a, RealFloat a) => Conjugate (Quaternion a) where
  conjugate (Quaternion e v) = Quaternion (conjugate e) (negate v)

reimagine :: RealFloat a => a -> a -> Quaternion a -> Quaternion a
reimagine r s (Quaternion _ v)
  | isNaN s || isInfinite s = let aux 0 = 0
                                  aux x = s * x
                              in Quaternion r (aux <$> v)
  | otherwise = Quaternion r (v^*s)

-- | quadrance of the imaginary component
qi :: Num a => Quaternion a -> a
qi (Quaternion _ v) = quadrance v

absi :: Floating a => Quaternion a -> a
absi = sqrt . qi

pow :: RealFloat a => Quaternion a -> a -> Quaternion a
pow q t = exp (t *^ log q)

-- ehh..
instance RealFloat a => Floating (Quaternion a) where
  {-# SPECIALIZE instance Floating (Quaternion Float) #-}
  {-# SPECIALIZE instance Floating (Quaternion Double) #-}
  pi = Quaternion pi 0
  exp q@(Quaternion e v)
    | qiq == 0 = Quaternion (exp e) v
    | ai <- sqrt qiq, ee <- exp e = reimagine (ee * cos ai) (ee * (sin ai / ai)) q
    where qiq = qi q
  log q@(Quaternion e v@(V3 _i j k))
    | qiq == 0 = if e >= 0
                 then Quaternion (log e) v
                 else Quaternion (log (negate e)) (V3 pi j k) -- mmm, pi
    | ai <- sqrt qiq, m <- sqrt (e*e + qiq) = reimagine (log m) (atan2 m e / ai) q
    where qiq = qi q
  x ** y = exp (y * log x)
  sqrt q@(Quaternion e v)
    | m   == 0 = q
    | qiq == 0 = if e > 0
                 then Quaternion (sqrt e) 0
                 else Quaternion 0 (V3 (sqrt (negate e)) 0 0)
    | im <- sqrt (0.5*(m-e)) / sqrt qiq = Quaternion (0.5*(m+e)) (v^*im)
    where qiq = qi q
          m = sqrt (e*e + qiq)
  cos q@(Quaternion e v)
    | qiq == 0 = Quaternion (cos e) v
    | ai <- sqrt qiq = reimagine (cos e * cosh ai) (- sin e * (sinh ai / ai)) q
    where qiq = qi q
  sin q@(Quaternion e v)
    | qiq == 0 = Quaternion (sin e) v
    | ai <- sqrt qiq = reimagine (sin e * cosh ai) (cos e * (sinh ai / ai)) q
    where qiq = qi q
  tan q@(Quaternion e v)
    | qiq == 0 = Quaternion (tan e) v
    | ai <- sqrt qiq, ce <- cos e, sai <- sinh ai, d <- ce*ce + sai*sai =
      reimagine (ce * sin e / d) (cosh ai * (sai / ai) / d) q
    where qiq = qi q
  sinh q@(Quaternion e v)
    | qiq == 0 = Quaternion (sinh e) v
    | ai <- sqrt qiq = reimagine (sinh e * cos ai) (cosh e * (sin ai / ai)) q
    where qiq = qi q
  cosh q@(Quaternion e v)
    | qiq == 0 = Quaternion (cosh e) v
    | ai <- sqrt qiq = reimagine (cosh e * cos ai) ((sinh e * sin ai) / ai) q
    where qiq = qi q
  tanh q@(Quaternion e v)
    | qiq == 0 = Quaternion (tanh e) v
    | ai <- sqrt qiq, se <- sinh e, cai <- cos ai, d <- se*se + cai*cai =
      reimagine ((cosh e * se) / d) ((cai * (sin ai / ai)) / d) q
    where qiq = qi q

  asin q = cut asin q
  acos q = cut acos q
  atan q = cut atan q

  asinh q = cut asinh q
  acosh q = cut acosh q
  atanh q = cut atanh q

cut :: RealFloat a => (Complex a -> Complex a) -> Quaternion a -> Quaternion a
cut f q@(Quaternion e v)
  | qiq == 0 = Quaternion a (_x.~b$v)
  | otherwise = reimagine a (b / ai) q
  where qiq = qi q
        ai = sqrt qiq
        a :+ b = f (e :+ ai)

cutWith :: RealFloat a => Complex a -> Quaternion a -> Quaternion a
cutWith (r :+ im) q@(Quaternion e v)
  | e /= 0 || qiq == 0 || isNaN qiq || isInfinite qiq = error "bad cut"
  | s <- im / sqrt qiq = Quaternion r (v^*s)
  where qiq = qi q

asinq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
asinq q@(Quaternion e _) u
  | qiq /= 0.0 || e >= -1 && e <= 1 = asin q
  | otherwise = cutWith (asin (e :+ sqrt qiq)) u
  where qiq = qi q

acosq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
acosq q@(Quaternion e _) u
  | qiq /= 0.0 || e >= -1 && e <= 1 = acos q
  | otherwise = cutWith (acos (e :+ sqrt qiq)) u
  where qiq = qi q

atanq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
atanq q@(Quaternion e _) u
  | e /= 0.0 || qiq >= -1 && qiq <= 1 = atan q
  | otherwise = cutWith (atan (e :+ sqrt qiq)) u
  where qiq = qi q

asinhq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
asinhq q@(Quaternion e _) u
  | e /= 0.0 || qiq >= -1 && qiq <= 1 = asinh q
  | otherwise = cutWith (asinh (e :+ sqrt qiq)) u
  where qiq = qi q

acoshq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
acoshq q@(Quaternion e _) u
  | qiq /= 0.0 || e >= 1 = asinh q
  | otherwise = cutWith (acosh (e :+ sqrt qiq)) u
  where qiq = qi q

atanhq :: RealFloat a => Quaternion a -> Quaternion a -> Quaternion a
atanhq q@(Quaternion e _) u
  | qiq /= 0.0 || e > -1 && e < 1 = atanh q
  | otherwise = cutWith (atanh (e :+ sqrt qiq)) u
  where qiq = qi q

slerp :: RealFloat a => Quaternion a -> Quaternion a -> a -> Quaternion a
slerp q p t
  | 1.0 - cosphi < 1e-8 = q
  | phi <- acos cosphi, r <- recip (sin phi)
  = (sin ((1-t)*phi)*r *^ q ^+^ f (sin (t*phi)*r) *^ p) ^/ sin phi
  where
   dqp = dot q p
   (cosphi, f) = if dqp < 0 then (-dqp, negate) else (dqp, id)
{-# SPECIALIZE slerp :: Quaternion Float -> Quaternion Float -> Float -> Quaternion Float #-}
{-# SPECIALIZE slerp :: Quaternion Double -> Quaternion Double -> Double -> Quaternion Double #-}

--slerp :: RealFloat a => Quaternion a -> Quaternion a -> a -> Quaternion a
--slerp q0 q1 = let q10 = q1 / q0 in \t -> pow q10 t * q0

-- |Apply a rotation to a vector.
rotate :: (Conjugate a, RealFloat a) => Quaternion a -> V3 a -> V3 a
rotate q v = (q * Quaternion 0 v * conjugate q)^._ijk

{-
rotate :: Num a => Quaternion a -> V3 a -> V3 a
rotate (Quaternion a' b c d) (V3 x y z) = V3
  (2*((t8+t10)*x+(t6- t4)*y+(t3+t7)*z)+x)
  (2*((t4+ t6)*y+(t5+t10)*y+(t9-t2)*z)+y)
  (2*((t7- t3)*z+(t2+ t9)*z+(t5+t8)*z)+z)
  where
    a = -a'
    t2 = a*b
    t3 = a*c
    t4 = a*d
    t5 = -b*b
    t6 = b*c
    t7 = b*d
    t8 = -c*c
    t9 = c*d
    t10 = -d*d
-}
{-# SPECIALIZE rotate :: Quaternion Float -> V3 Float -> V3 Float #-}
{-# SPECIALIZE rotate :: Quaternion Double -> V3 Double -> V3 Double #-}

instance (RealFloat a, Epsilon a) => Epsilon (Quaternion a) where
  nearZero = nearZero . quadrance

-- |@axisAngle axis theta@ builds a 'Quaternion' representing a
-- rotation of @theta@ radians about @axis@.
axisAngle :: (Epsilon a, Floating a) => V3 a -> a -> Quaternion a
axisAngle axis theta = normalize $ Quaternion (cos half) $ (sin half) *^ axis
  where half = theta / 2

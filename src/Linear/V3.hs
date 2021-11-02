{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}

#ifndef MIN_VERSION_hashable
#define MIN_VERSION_hashable(x,y,z) 1
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
-- 3-D Vectors
----------------------------------------------------------------------------
module Linear.V3
  ( V3(..)
  , cross, triple
  , R1(..)
  , R2(..)
  , _yx
  , R3(..)
  , _xz, _yz, _zx, _zy
  , _xzy, _yxz, _yzx, _zxy, _zyx
  , ex, ey, ez
  ) where

import Control.Applicative
import Control.DeepSeq (NFData(rnf))
import Control.Monad (liftM)
import Control.Monad.Fix
import Control.Monad.Zip
import Control.Lens as Lens hiding ((<.>))
import Data.Binary as Binary -- binary
import Data.Bytes.Serial -- bytes
import Data.Data
import Data.Distributive
import Data.Foldable
import qualified Data.Foldable.WithIndex as WithIndex
import Data.Functor.Bind
import Data.Functor.Classes
import Data.Functor.Rep
import qualified Data.Functor.WithIndex as WithIndex
import Data.Hashable
import Data.Hashable.Lifted
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Semigroup.Foldable
import Data.Serialize as Cereal -- cereal
import qualified Data.Traversable.WithIndex as WithIndex
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import GHC.Arr (Ix(..))
import GHC.Generics (Generic, Generic1)
#if defined(MIN_VERSION_template_haskell)
import Language.Haskell.TH.Syntax (Lift)
#endif
import Linear.Epsilon
import Linear.Metric
import Linear.V
import Linear.V2
import Linear.Vector
import System.Random (Random(..))

-- $setup
-- >>> import Control.Lens hiding (index)

-- | A 3-dimensional vector
data V3 a = V3 !a !a !a deriving (Eq,Ord,Show,Read,Data
                                 ,Generic,Generic1
#if defined(MIN_VERSION_template_haskell)
                                 ,Lift
#endif
                                 )

instance Finite V3 where
  type Size V3 = 3
  toV (V3 a b c) = V (V.fromListN 3 [a,b,c])
  fromV (V v) = V3 (v V.! 0) (v V.! 1) (v V.! 2)

instance Functor V3 where
  fmap f (V3 a b c) = V3 (f a) (f b) (f c)
  {-# INLINE fmap #-}
  a <$ _ = V3 a a a
  {-# INLINE (<$) #-}

instance Foldable V3 where
  foldMap f (V3 a b c) = f a `mappend` f b `mappend` f c
  {-# INLINE foldMap #-}
  null _ = False
  length _ = 3

instance Random a => Random (V3 a) where
  random g = case random g of
    (a, g') -> case random g' of
      (b, g'') -> case random g'' of
        (c, g''') -> (V3 a b c, g''')
  randomR (V3 a b c, V3 a' b' c') g = case randomR (a,a') g of
    (a'', g') -> case randomR (b,b') g' of
      (b'', g'') -> case randomR (c,c') g'' of
        (c'', g''') -> (V3 a'' b'' c'', g''')

instance Traversable V3 where
  traverse f (V3 a b c) = V3 <$> f a <*> f b <*> f c
  {-# INLINE traverse #-}

instance Foldable1 V3 where
  foldMap1 f (V3 a b c) = f a <> f b <> f c
  {-# INLINE foldMap1 #-}

instance Traversable1 V3 where
  traverse1 f (V3 a b c) = V3 <$> f a <.> f b <.> f c
  {-# INLINE traverse1 #-}

instance Apply V3 where
  V3 a b c <.> V3 d e f = V3 (a d) (b e) (c f)
  {-# INLINE (<.>) #-}

instance Applicative V3 where
  pure a = V3 a a a
  {-# INLINE pure #-}
  V3 a b c <*> V3 d e f = V3 (a d) (b e) (c f)
  {-# INLINE (<*>) #-}

instance Additive V3 where
  zero = pure 0
  {-# INLINE zero #-}
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

instance Bind V3 where
  V3 a b c >>- f = V3 a' b' c' where
    V3 a' _ _ = f a
    V3 _ b' _ = f b
    V3 _ _ c' = f c
  {-# INLINE (>>-) #-}

instance Monad V3 where
#if !(MIN_VERSION_base(4,11,0))
  return a = V3 a a a
  {-# INLINE return #-}
#endif
  V3 a b c >>= f = V3 a' b' c' where
    V3 a' _ _ = f a
    V3 _ b' _ = f b
    V3 _ _ c' = f c
  {-# INLINE (>>=) #-}

instance Num a => Num (V3 a) where
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

instance Fractional a => Fractional (V3 a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Floating a => Floating (V3 a) where
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

instance Hashable a => Hashable (V3 a) where
  hashWithSalt s (V3 a b c) = s `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c
  {-# INLINE hashWithSalt #-}

instance Hashable1 V3 where
  liftHashWithSalt h s (V3 a b c) = s `h` a `h` b `h` c
  {-# INLINE liftHashWithSalt #-}

instance Metric V3 where
  dot (V3 a b c) (V3 d e f) = a * d + b * e + c * f
  {-# INLINABLE dot #-}

instance Distributive V3 where
  distribute f = V3 (fmap (\(V3 x _ _) -> x) f) (fmap (\(V3 _ y _) -> y) f) (fmap (\(V3 _ _ z) -> z) f)
  {-# INLINE distribute #-}

-- | A space that distinguishes 3 orthogonal basis vectors: '_x', '_y', and '_z'. (It may have more)
class R2 t => R3 t where
  -- |
  -- >>> V3 1 2 3 ^. _z
  -- 3
  _z :: Lens' (t a) a

  _xyz :: Lens' (t a) (V3 a)

_xz, _yz, _zx, _zy :: R3 t => Lens' (t a) (V2 a)

_xz f = _xyz $ \(V3 a b c) -> f (V2 a c) <&> \(V2 a' c') -> V3 a' b c'
{-# INLINE _xz #-}

_yz f = _xyz $ \(V3 a b c) -> f (V2 b c) <&> \(V2 b' c') -> V3 a b' c'
{-# INLINE _yz #-}

_zx f = _xyz $ \(V3 a b c) -> f (V2 c a) <&> \(V2 c' a') -> V3 a' b c'
{-# INLINE _zx #-}

_zy f = _xyz $ \(V3 a b c) -> f (V2 c b) <&> \(V2 c' b') -> V3 a b' c'
{-# INLINE _zy #-}

_xzy, _yxz, _yzx, _zxy, _zyx :: R3 t => Lens' (t a) (V3 a)

_xzy f = _xyz $ \(V3 a b c) -> f (V3 a c b) <&> \(V3 a' c' b') -> V3 a' b' c'
{-# INLINE _xzy #-}

_yxz f = _xyz $ \(V3 a b c) -> f (V3 b a c) <&> \(V3 b' a' c') -> V3 a' b' c'
{-# INLINE _yxz #-}

_yzx f = _xyz $ \(V3 a b c) -> f (V3 b c a) <&> \(V3 b' c' a') -> V3 a' b' c'
{-# INLINE _yzx #-}

_zxy f = _xyz $ \(V3 a b c) -> f (V3 c a b) <&> \(V3 c' a' b') -> V3 a' b' c'
{-# INLINE _zxy #-}

_zyx f = _xyz $ \(V3 a b c) -> f (V3 c b a) <&> \(V3 c' b' a') -> V3 a' b' c'
{-# INLINE _zyx #-}

ez :: R3 t => E t
ez = E _z

instance R1 V3 where
  _x f (V3 a b c) = (\a' -> V3 a' b c) <$> f a
  {-# INLINE _x #-}

instance R2 V3 where
  _y f (V3 a b c) = (\b' -> V3 a b' c) <$> f b
  {-# INLINE _y #-}
  _xy f (V3 a b c) = (\(V2 a' b') -> V3 a' b' c) <$> f (V2 a b)
  {-# INLINE _xy #-}

instance R3 V3 where
  _z f (V3 a b c) = V3 a b <$> f c
  {-# INLINE _z #-}
  _xyz = id
  {-# INLINE _xyz #-}

instance Storable a => Storable (V3 a) where
  sizeOf _ = 3 * sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (V3 x y z) = do poke ptr' x
                           pokeElemOff ptr' 1 y
                           pokeElemOff ptr' 2 z
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = V3 <$> peek ptr' <*> peekElemOff ptr' 1 <*> peekElemOff ptr' 2
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

-- | cross product
cross :: Num a => V3 a -> V3 a -> V3 a
cross (V3 a b c) (V3 d e f) = V3 (b*f-c*e) (c*d-a*f) (a*e-b*d)
{-# INLINABLE cross #-}

-- | scalar triple product
triple :: Num a => V3 a -> V3 a -> V3 a -> a
triple a b c = dot a (cross b c)
{-# INLINE triple #-}

instance Epsilon a => Epsilon (V3 a) where
  nearZero = nearZero . quadrance
  {-# INLINE nearZero #-}

instance Ix a => Ix (V3 a) where
  {-# SPECIALISE instance Ix (V3 Int) #-}

  range (V3 l1 l2 l3,V3 u1 u2 u3) =
      [V3 i1 i2 i3 | i1 <- range (l1,u1)
                   , i2 <- range (l2,u2)
                   , i3 <- range (l3,u3)
                   ]
  {-# INLINE range #-}

  unsafeIndex (V3 l1 l2 l3,V3 u1 u2 u3) (V3 i1 i2 i3) =
    unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
    unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) *
    unsafeIndex (l1,u1) i1)
  {-# INLINE unsafeIndex #-}

  inRange (V3 l1 l2 l3,V3 u1 u2 u3) (V3 i1 i2 i3) =
    inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
    inRange (l3,u3) i3
  {-# INLINE inRange #-}

instance Representable V3 where
  type Rep V3 = E V3
  tabulate f = V3 (f ex) (f ey) (f ez)
  {-# INLINE tabulate #-}
  index xs (E l) = view l xs
  {-# INLINE index #-}

instance WithIndex.FunctorWithIndex (E V3) V3 where
  imap f (V3 a b c) = V3 (f ex a) (f ey b) (f ez c)
  {-# INLINE imap #-}

instance WithIndex.FoldableWithIndex (E V3) V3 where
  ifoldMap f (V3 a b c) = f ex a `mappend` f ey b `mappend` f ez c
  {-# INLINE ifoldMap #-}

instance WithIndex.TraversableWithIndex (E V3) V3 where
  itraverse f (V3 a b c) = V3 <$> f ex a <*> f ey b <*> f ez c
  {-# INLINE itraverse #-}

#if !MIN_VERSION_lens(5,0,0)
instance Lens.FunctorWithIndex     (E V3) V3 where imap      = WithIndex.imap
instance Lens.FoldableWithIndex    (E V3) V3 where ifoldMap  = WithIndex.ifoldMap
instance Lens.TraversableWithIndex (E V3) V3 where itraverse = WithIndex.itraverse
#endif

type instance Index (V3 a) = E V3
type instance IxValue (V3 a) = a

instance Ixed (V3 a) where
  ix i = el i
  {-# INLINE ix #-}

instance Each (V3 a) (V3 b) a b where
  each = traverse
  {-# INLINE each #-}

data instance U.Vector    (V3 a) =  V_V3 {-# UNPACK #-} !Int !(U.Vector    a)
data instance U.MVector s (V3 a) = MV_V3 {-# UNPACK #-} !Int !(U.MVector s a)
instance U.Unbox a => U.Unbox (V3 a)

instance U.Unbox a => M.MVector U.MVector (V3 a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  basicLength (MV_V3 n _) = n
  basicUnsafeSlice m n (MV_V3 _ v) = MV_V3 n (M.basicUnsafeSlice (3*m) (3*n) v)
  basicOverlaps (MV_V3 _ v) (MV_V3 _ u) = M.basicOverlaps v u
  basicUnsafeNew n = liftM (MV_V3 n) (M.basicUnsafeNew (3*n))
  basicUnsafeRead (MV_V3 _ v) i =
    do let o = 3*i
       x <- M.basicUnsafeRead v o
       y <- M.basicUnsafeRead v (o+1)
       z <- M.basicUnsafeRead v (o+2)
       return (V3 x y z)
  basicUnsafeWrite (MV_V3 _ v) i (V3 x y z) =
    do let o = 3*i
       M.basicUnsafeWrite v o     x
       M.basicUnsafeWrite v (o+1) y
       M.basicUnsafeWrite v (o+2) z
  basicInitialize (MV_V3 _ v) = M.basicInitialize v
  {-# INLINE basicInitialize #-}

instance U.Unbox a => G.Vector U.Vector (V3 a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze (MV_V3 n v) = liftM ( V_V3 n) (G.basicUnsafeFreeze v)
  basicUnsafeThaw   ( V_V3 n v) = liftM (MV_V3 n) (G.basicUnsafeThaw   v)
  basicLength       ( V_V3 n _) = n
  basicUnsafeSlice m n (V_V3 _ v) = V_V3 n (G.basicUnsafeSlice (3*m) (3*n) v)
  basicUnsafeIndexM (V_V3 _ v) i =
    do let o = 3*i
       x <- G.basicUnsafeIndexM v o
       y <- G.basicUnsafeIndexM v (o+1)
       z <- G.basicUnsafeIndexM v (o+2)
       return (V3 x y z)

instance MonadZip V3 where
  mzipWith = liftA2

instance MonadFix V3 where
  mfix f = V3 (let V3 a _ _ = f a in a)
              (let V3 _ a _ = f a in a)
              (let V3 _ _ a = f a in a)

instance Bounded a => Bounded (V3 a) where
  minBound = pure minBound
  {-# INLINE minBound #-}
  maxBound = pure maxBound
  {-# INLINE maxBound #-}

instance NFData a => NFData (V3 a) where
  rnf (V3 a b c) = rnf a `seq` rnf b `seq` rnf c

instance Serial1 V3 where
  serializeWith = traverse_
  deserializeWith k = V3 <$> k <*> k <*> k

instance Serial a => Serial (V3 a) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance Binary a => Binary (V3 a) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance Serialize a => Serialize (V3 a) where
  put = serializeWith Cereal.put
  get = deserializeWith Cereal.get

instance Eq1 V3 where
  liftEq k (V3 a b c) (V3 d e f) = k a d && k b e && k c f
instance Ord1 V3 where
  liftCompare k (V3 a b c) (V3 d e f) = k a d `mappend` k b e `mappend` k c f
instance Read1 V3 where
  liftReadsPrec k _ d = readParen (d > 10) $ \r ->
     [ (V3 a b c, r4)
     | ("V3",r1) <- lex r
     , (a,r2) <- k 11 r1
     , (b,r3) <- k 11 r2
     , (c,r4) <- k 11 r3
     ]
instance Show1 V3 where
  liftShowsPrec f _ d (V3 a b c) = showParen (d > 10) $
     showString "V3 " . f 11 a . showChar ' ' . f 11 b . showChar ' ' . f 11 c

instance Field1 (V3 a) (V3 a) a a where
  _1 f (V3 x y z) = f x <&> \x' -> V3 x' y z

instance Field2 (V3 a) (V3 a) a a where
  _2 f (V3 x y z) = f y <&> \y' -> V3 x y' z

instance Field3 (V3 a) (V3 a) a a where
  _3 f (V3 x y z) = f z <&> \z' -> V3 x y z'

instance Semigroup a => Semigroup (V3 a) where
 (<>) = liftA2 (<>)

instance Monoid a => Monoid (V3 a) where
  mempty = pure mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = liftA2 mappend
#endif


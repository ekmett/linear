{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_hashable
#define MIN_VERSION_hashable
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- 0-D Vectors
----------------------------------------------------------------------------
module Linear.V0
  ( V0(..)
  ) where

import Control.Applicative
import Control.DeepSeq (NFData(rnf))
import Control.Lens
import Control.Monad.Fix
import Control.Monad.Zip
import Data.Data
import Data.Distributive
import Data.Foldable
import Data.Functor.Rep
import Data.Functor.Bind
import Data.Hashable
import Data.Ix
import Data.Semigroup
import Foreign.Storable (Storable(..))
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
#endif
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U
import Linear.Metric
import Linear.Epsilon
import Linear.Vector
import Prelude hiding (sum)

-- $setup
-- >>> import Control.Lens

-- | A 0-dimensional vector
--
-- >>> pure 1 :: V0 Int
-- V0
--
-- >>> V0 + V0
-- V0
--
data V0 a = V0 deriving (Eq,Ord,Show,Read,Ix,Enum,Data,Typeable
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
                        ,Generic
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
                        ,Generic1
#endif
                        )

instance Functor V0 where
  fmap _ V0 = V0
  {-# INLINE fmap #-}
  _ <$ _ = V0
  {-# INLINE (<$) #-}

instance Foldable V0 where
  foldMap _ V0 = mempty
  {-# INLINE foldMap #-}

instance Traversable V0 where
  traverse _ V0 = pure V0
  {-# INLINE traverse #-}

instance Apply V0 where
  V0 <.> V0 = V0
  {-# INLINE (<.>) #-}

instance Applicative V0 where
  pure _ = V0
  {-# INLINE pure #-}
  V0 <*> V0 = V0
  {-# INLINE (<*>) #-}

instance Additive V0 where
  zero = V0
  {-# INLINE zero #-}
  liftU2 _ V0 V0 = V0
  {-# INLINE liftU2 #-}
  liftI2 _ V0 V0 = V0
  {-# INLINE liftI2 #-}

instance Bind V0 where
  V0 >>- _ = V0
  {-# INLINE (>>-) #-}

instance Monad V0 where
  return _ = V0
  {-# INLINE return #-}
  V0 >>= _ = V0
  {-# INLINE (>>=) #-}

instance Num (V0 a) where
  V0 + V0 = V0
  {-# INLINE (+) #-}
  V0 - V0 = V0
  {-# INLINE (-) #-}
  V0 * V0 = V0
  {-# INLINE (*) #-}
  negate V0 = V0
  {-# INLINE negate #-}
  abs V0 = V0
  {-# INLINE abs #-}
  signum V0 = V0
  {-# INLINE signum #-}
  fromInteger _ = V0
  {-# INLINE fromInteger #-}

instance Fractional (V0 a) where
  recip _ = V0
  {-# INLINE recip #-}
  V0 / V0 = V0
  {-# INLINE (/) #-}
  fromRational _ = V0
  {-# INLINE fromRational #-}

instance Floating (V0 a) where
    pi = V0
    {-# INLINE pi #-}
    exp V0 = V0
    {-# INLINE exp #-}
    sqrt V0 = V0
    {-# INLINE sqrt #-}
    log V0 = V0
    {-# INLINE log #-}
    V0 ** V0 = V0
    {-# INLINE (**) #-}
    logBase V0 V0 = V0
    {-# INLINE logBase #-}
    sin V0 = V0
    {-# INLINE sin #-}
    tan V0 = V0
    {-# INLINE tan #-}
    cos V0 = V0
    {-# INLINE cos #-}
    asin V0 = V0
    {-# INLINE asin #-}
    atan V0 = V0
    {-# INLINE atan #-}
    acos V0 = V0
    {-# INLINE acos #-}
    sinh V0 = V0
    {-# INLINE sinh #-}
    tanh V0 = V0
    {-# INLINE tanh #-}
    cosh V0 = V0
    {-# INLINE cosh #-}
    asinh V0 = V0
    {-# INLINE asinh #-}
    atanh V0 = V0
    {-# INLINE atanh #-}
    acosh V0 = V0
    {-# INLINE acosh #-}

instance Metric V0 where
  dot V0 V0 = 0
  {-# INLINE dot #-}

instance Distributive V0 where
  distribute _ = V0
  {-# INLINE distribute #-}

instance Hashable (V0 a) where
#if (MIN_VERSION_hashable(1,2,1)) || !(MIN_VERSION_hashable(1,2,0))
  hash V0 = 0
  {-# INLINE hash #-}
#endif
  hashWithSalt s V0 = s
  {-# INLINE hashWithSalt #-}

instance Epsilon a => Epsilon (V0 a) where
  nearZero _ = True
  {-# INLINE nearZero #-}

instance Storable a => Storable (V0 a) where
  sizeOf _ = 0
  {-# INLINE sizeOf #-}
  alignment _ = 1
  {-# INLINE alignment #-}
  poke _ V0 = return ()
  {-# INLINE poke #-}
  peek _ = return V0
  {-# INLINE peek #-}

instance FunctorWithIndex (E V0) V0 where
  imap _ V0 = V0
  {-# INLINE imap #-}

instance FoldableWithIndex (E V0) V0 where
  ifoldMap _ V0 = mempty
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex (E V0) V0 where
  itraverse _ V0 = pure V0
  {-# INLINE itraverse #-}

instance Representable V0 where
  type Rep V0 = E V0
  tabulate _ = V0
  {-# INLINE tabulate #-}
  index xs (E l) = view l xs
  {-# INLINE index #-}

type instance Index (V0 a) = E V0
type instance IxValue (V0 a) = a

instance Ixed (V0 a) where
  ix = el
  {-# INLINE ix #-}

instance Each (V0 a) (V0 b) a b where
  each = traverse
  {-# INLINE each #-}

newtype instance U.Vector    (V0 a) = V_V0 Int
newtype instance U.MVector s (V0 a) = MV_V0 Int
instance U.Unbox (V0 a)

instance M.MVector U.MVector (V0 a) where
  basicLength (MV_V0 n) = n
  basicUnsafeSlice _ n _ = MV_V0 n
  basicOverlaps _ _ = False
  basicUnsafeNew n = return (MV_V0 n)
  basicUnsafeRead _ _ = return V0
  basicUnsafeWrite _ _ _ = return ()

instance G.Vector U.Vector (V0 a) where
  basicUnsafeFreeze (MV_V0 n) = return (V_V0 n)
  basicUnsafeThaw (V_V0 n) = return (MV_V0 n)
  basicLength (V_V0 n) = n
  basicUnsafeSlice _ n _ = V_V0 n
  basicUnsafeIndexM _ _ = return V0

instance MonadZip V0 where
  mzip V0 V0 = V0
  mzipWith _ V0 V0 = V0
  munzip V0 = (V0, V0)

instance MonadFix V0 where
  mfix _ = V0

instance Bounded (V0 a) where
  minBound = V0
  {-# INLINE minBound #-}
  maxBound = V0
  {-# INLINE maxBound #-}

instance NFData (V0 a) where
  rnf V0 = ()

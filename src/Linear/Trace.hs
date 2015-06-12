{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
---------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Simple matrix operation for low-dimensional primitives.
---------------------------------------------------------------------------
module Linear.Trace
  ( Trace(..)
  ) where

import Control.Monad as Monad
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Plucker
import Linear.Quaternion
import Linear.V
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 704
import Data.Complex
#endif
import Data.Distributive
import Data.Foldable as Foldable
import Data.Functor.Bind as Bind
import Data.Functor.Compose
import Data.Functor.Product
import Data.Hashable
import Data.HashMap.Lazy
import Data.IntMap
import Data.Map

-- $setup
-- >>> import Data.Complex
-- >>> import Data.IntMap
-- >>> import Debug.SimpleReflect.Vars

class Functor m => Trace m where
  -- | Compute the trace of a matrix
  --
  -- >>> trace (V2 (V2 a b) (V2 c d))
  -- a + d
  trace :: Num a => m (m a) -> a
#ifndef HLINT
  default trace :: (Foldable m, Num a) => m (m a) -> a
  trace = Foldable.sum . diagonal
  {-# INLINE trace #-}
#endif

  -- | Compute the diagonal of a matrix
  --
  -- >>> diagonal (V2 (V2 a b) (V2 c d))
  -- V2 a d
  diagonal :: m (m a) -> m a
#ifndef HLINT
  default diagonal :: Monad m => m (m a) -> m a
  diagonal = Monad.join
  {-# INLINE diagonal #-}
#endif

instance Trace IntMap where
  diagonal = Bind.join
  {-# INLINE diagonal #-}

instance Ord k => Trace (Map k) where
  diagonal = Bind.join
  {-# INLINE diagonal #-}

instance (Eq k, Hashable k) => Trace (HashMap k) where
  diagonal = Bind.join
  {-# INLINE diagonal #-}

instance Dim n => Trace (V n)
instance Trace V0
instance Trace V1
instance Trace V2
instance Trace V3
instance Trace V4
instance Trace Plucker
instance Trace Quaternion

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 704
instance Trace Complex where
  trace ((a :+ _) :+ (_ :+ b)) = a + b
  {-# INLINE trace #-}
  diagonal ((a :+ _) :+ (_ :+ b)) = a :+ b
  {-# INLINE diagonal #-}
#endif

instance (Trace f, Trace g) => Trace (Product f g) where
  trace (Pair xx yy) = trace (pfst <$> xx) + trace (psnd <$> yy) where
    pfst (Pair x _) = x
    psnd (Pair _ y) = y
  {-# INLINE trace #-}
  diagonal (Pair xx yy) = diagonal (pfst <$> xx) `Pair` diagonal (psnd <$> yy) where
    pfst (Pair x _) = x
    psnd (Pair _ y) = y
  {-# INLINE diagonal #-}

instance (Distributive g, Trace g, Trace f) => Trace (Compose g f) where
  trace = trace . fmap (fmap trace . distribute) . getCompose . fmap getCompose
  {-# INLINE trace #-}
  diagonal = Compose . fmap diagonal . diagonal . fmap distribute . getCompose . fmap getCompose
  {-# INLINE diagonal #-}

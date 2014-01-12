{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Corepresentable functors as vector spaces
----------------------------------------------------------------------------
module Linear.Core
  ( Core(..)
  , column
  ) where

import Control.Applicative
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
#endif

type LensLike f s t a b = (a -> f b) -> s -> f t
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- |
-- A 'Functor' @f@ is corepresentable if it is isomorphic to @(x -> a)@
-- for some x. Nearly all such functors can be represented by choosing @x@ to be
-- the set of lenses that are polymorphic in the contents of the 'Functor',
-- that is to say @x = 'Rep' f@ is a valid choice of 'x' for (nearly) every
-- 'Representable' 'Functor'.
class Functor f => Core f where
  -- | Form a structure by applying the given function to lenses focused on its holes.
  core :: ((forall x. Lens' (f x) x) -> a) -> f a

data Context a b t = Context { peek :: b -> t, pos :: a }
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
                   deriving (Generic, Generic1)
#elif defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
                   deriving (Generic)
#endif

instance Functor (Context a b) where
  fmap f (Context bt a) = Context (f.bt) a

view :: LensLike (Const a) s t a b -> s -> a
view l = getConst . l Const

-- | This is a generalization of 'Control.Lens.inside' to work over any corepresentable 'Functor'.
--
-- @
-- 'column' :: 'Core' f => 'Lens' s t a b -> 'Lens' (f s) (f t) (f a) (f b)
-- @
--
-- In practice it is used to access a column of a matrix.
--
--
-- >>> V2 (V3 1 2 3) (V3 4 5 6) ^._x
-- V3 1 2 3
--
-- >>> V2 (V3 1 2 3) (V3 4 5 6) ^.column _x
-- V2 1 4
column :: Core f => LensLike (Context a b) s t a b -> Lens (f s) (f t) (f a) (f b)
column l f es = o <$> f i where
   go = l (Context id)
   i = core $ \ e -> pos $ go (view e es)
   o eb = core $ \ e -> peek (go (view e es)) (view e eb)

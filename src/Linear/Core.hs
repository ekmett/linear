{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
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
  , coreE
  , E(..)
  , column
  , imapCore
  ) where

import Control.Applicative
import Control.Lens
import Control.Lens.Internal.Context

-- $setup
-- >>> import Linear
-- >>> import Control.Lens

-- |
-- A 'Functor' @f@ is corepresentable if it is isomorphic to @(x -> a)@
-- for some x. Nearly all such functors can be represented by choosing @x@ to be
-- the set of lenses that are polymorphic in the contents of the 'Functor',
-- that is to say @x = 'Rep' f@ is a valid choice of 'x' for (nearly) every
-- 'Representable' 'Functor'.
class Functor f => Core f where
  -- | Form a structure by applying the given function to lenses focused on its holes.
  core :: ((forall x. Lens' (f x) x) -> a) -> f a

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
   i = core $ \ e -> ipos $ go (view e es)
   o eb = core $ \ e -> ipeek (view e eb) (go (view e es))

-- | Basis element
newtype E t = E { el :: forall x. Lens' (t x) x }

coreE :: Core f => (E f -> a) -> f a
coreE f = core $ \x -> f (E x)

imapCore :: Core f => (E f -> a -> b) -> f a -> f b
imapCore f xs = core $ \l -> f (E l) (xs^.l)
{-# INLINE imapCore #-}

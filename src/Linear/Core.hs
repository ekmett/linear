{-# LANGUAGE RankNTypes #-}
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
  , incore
  ) where

import Control.Applicative

-- |
-- A 'Functor' @f@ is corepresentable if it is isomorphic to @(x -> a)@
-- for some x. Nearly all such functors can be represented by choosing @x@ to be
-- the set of lenses that are polymorphic in the contents of the 'Functor',
-- that is to say @x = 'Rep' f@ is a valid choice of 'x' for (nearly) every
-- 'Representable' 'Functor'.
class Functor f => Core f where
  -- | Form a structure by applying the given function to lenses focused on its holes.
  --
  -- @
  -- 'core' :: ((forall x. 'Control.Lens.Lens' (f x) x) -> a) -> f a
  -- @
  core :: ((forall g x. Functor g => (x -> g x) -> f x -> g (f x)) -> a) -> f a

data Context a b t = Context { peek :: b -> t, pos :: a }

instance Functor (Context a b) where
  fmap f (Context bt a) = Context (f.bt) a

view :: ((a -> Const a b) -> s -> Const a t) -> s -> a
view l = getConst . l Const

-- | This is a generalization of 'Control.Lens.inside' to work over any corepresentable 'Functor'.
--
-- @
-- 'incore' :: 'Core' f => 'Lens' s t a b -> 'Lens' (f s) (f t) (f a) (f b)
-- @
incore :: (Functor g, Core f) => ((a -> Context a b b) -> s -> Context a b t) -> (f a -> g (f b)) -> f s -> g (f t)
incore l f es = o <$> f i where
   go = l (Context id)
   i = core $ \ e -> pos $ go (view e es)
   o eb = core $ \ e -> peek (go (view e es)) (view e eb)

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
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
  ( core
  , E(..)
  ) where

import Control.Lens hiding (index)
import Data.Functor.Rep

-- | Basis element
newtype E t = E { el :: forall x. Lens' (t x) x }

-- $setup
-- >>> import Linear
-- >>> import Control.Lens

-- |
-- A 'Functor' @f@ is corepresentable if it is isomorphic to @(x -> a)@
-- for some x. Nearly all such functors can be represented by choosing @x@ to be
-- the set of lenses that are polymorphic in the contents of the 'Functor',
-- that is to say @x = 'Rep' f@ is a valid choice of 'x' for (nearly) every
-- 'Representable' 'Functor'.
--
-- Form a structure by applying the given function to lenses focused on its holes.

core :: (Representable f, Rep f ~ E f) => ((forall x. Lens' (f x) x) -> a) -> f a
core k = tabulate $ \x -> k (el x)
{-# INLINE core #-}

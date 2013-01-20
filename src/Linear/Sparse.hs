-----------------------------------------------------------------------------
-- |
-- Module      :  Linear.Sparse
-- Copyright   :  (C) 2013 John Wiegley,
--                (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  John Wiegley <johnw@fpcomplete.com>,
--                Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Representation and operations on sparse vector spaces and matrices
----------------------------------------------------------------------------
module Linear.Sparse where

import Control.Applicative
import Control.Monad (join)
import Data.Distributive
import Data.Foldable as Foldable
import Linear.Epsilon
import Linear.Metric
import Linear.Vector ((*^))
import Linear.Conjugate

{- Content goes here. -}
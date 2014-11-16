{-# LANGUAGE GADTs #-}
---------------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-2014 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Utility for working with Plücker coordinates for lines in 3d homogeneous space.
----------------------------------------------------------------------------------
module Linear.Plucker.Coincides
  ( Coincides(..)
  ) where

import Linear.Epsilon
import Linear.Plucker

-- | When lines are represented as Plücker coordinates, we have the
-- ability to check for both directed and undirected
-- equality. Undirected equality between 'Line's (or a 'Line' and a
-- 'Ray') checks that the two lines coincide in 3D space. Directed
-- equality, between two 'Ray's, checks that two lines coincide in 3D,
-- and have the same direction. To accomodate these two notions of
-- equality, we use an 'Eq' instance on the 'Coincides' data type.
--
-- For example, to check the /directed/ equality between two lines,
-- @p1@ and @p2@, we write, @Ray p1 == Ray p2@.
data Coincides a where
  Line :: (Epsilon a, Fractional a) => Plucker a -> Coincides a
  Ray  :: (Epsilon a, Fractional a, Ord a) => Plucker a -> Coincides a

instance Eq (Coincides a) where
  Line a == Line b  = coincides a b
  Line a == Ray b   = coincides a b
  Ray a  == Line b  = coincides a b
  Ray a  == Ray b   = coincides' a b

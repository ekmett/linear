---------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Common perspective transformation matrices.
---------------------------------------------------------------------------
module Linear.Perspective
  ( lookAt
  ) where

import Control.Lens hiding (index)
import Linear.V3
import Linear.V4
import Linear.Matrix
import Linear.Epsilon
import Linear.Metric

-- | Build a look at view matrix
lookAt :: (Epsilon a, Floating a) => V3 a -> V3 a -> V3 a -> M44 a
lookAt eye center up =
  V4 (V4 (xa ^. _x)     (xa ^. _y)     (xa ^. _z)     xd)
     (V4 (ya ^. _x)     (ya ^. _y)     (ya ^. _z)     yd)
     (V4 (-(za ^. _x))  (-(za ^. _y))  (-(za ^. _z))  zd)
     (V4 0              0              0              1)
  where za = normalize $ center - eye
        xa = normalize $ cross za up
        ya = cross xa za
        xd = -(dot xa eye)
        yd = -(dot ya eye)
        zd = (dot za eye)

---------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2014 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Common projection matrices: e.g. perspective/orthographic transformation
-- matrices.
---------------------------------------------------------------------------
module Linear.Projection
  ( lookAt
  , perspective
  , inversePerspective
  , infinitePerspective
  , inverseInfinitePerspective
  , frustum
  , inverseFrustum
  , ortho
  ) where

import Control.Lens hiding (index)
import Linear.V3
import Linear.V4
import Linear.Matrix
import Linear.Epsilon
import Linear.Metric

-- | Build a look at view matrix
lookAt
  :: (Epsilon a, Floating a)
  => V3 a -- ^ Eye
  -> V3 a -- ^ Center
  -> V3 a -- ^ Up
  -> M44 a
lookAt eye center up =
  V4 (V4 (xa^._x)  (xa^._y)  (xa^._z)  xd)
     (V4 (ya^._x)  (ya^._y)  (ya^._z)  yd)
     (V4 (-za^._x) (-za^._y) (-za^._z) zd)
     (V4 0         0         0          1)
  where za = normalize $ center - eye
        xa = normalize $ cross za up
        ya = cross xa za
        xd = -dot xa eye
        yd = -dot ya eye
        zd = dot za eye

-- | Build a matrix for a symmetric perspective-view frustum
perspective
  :: Floating a
  => a -- ^ FOV
  -> a -- ^ Aspect ratio
  -> a -- ^ Near plane
  -> a -- ^ Far plane
  -> M44 a
perspective fovy aspect near far =
  V4 (V4 x 0 0    0)
     (V4 0 y 0    0)
     (V4 0 0 z    w)
     (V4 0 0 (-1) 0)
  where tanHalfFovy = tan $ fovy / 2
        x = 1 / (aspect * tanHalfFovy)
        y = 1 / tanHalfFovy
        z = -(far + near) / (far - near)
        w = -(2 * far * near) / (far - near)

-- | Build an inverse perspective matrix
inversePerspective
  :: Floating a
  => a -- ^ FOV
  -> a -- ^ Aspect ratio
  -> a -- ^ Near plane
  -> a -- ^ Far plane
  -> M44 a
inversePerspective fovy aspect near far =
  V4 (V4 a 0 0 0   )
     (V4 0 b 0 0   )
     (V4 0 0 0 (-1))
     (V4 0 0 c d   )
  where tanHalfFovy = tan $ fovy / 2
        a = aspect * tanHalfFovy
        b = tanHalfFovy
        c = -(far - near) / (2 * far * near)
        d = (far + near) / (2 * far * near)
 

-- | Build a perspective matrix per the classic @glFrustum@ arguments.
frustum
  :: Floating a
  => a -- ^ left
  -> a -- ^ right
  -> a -- ^ bottom
  -> a -- ^ top
  -> a -- ^ near
  -> a -- ^ far
  -> M44 a
frustum l r b t n f = 
  V4 (V4 x 0 a    0)
     (V4 0 y e    0)
     (V4 0 0 c    d)
     (V4 0 0 (-1) 0)
  where
    rml = r-l 
    tmb = t-b
    fmn = f-n
    x = 2*n/rml
    y = 2*n/tmb
    a = (r+l)/rml
    e = (t+b)/tmb
    c = negate (f+n)/fmn
    d = (-2*f*n)/fmn

inverseFrustum
  :: Floating a
  => a -- ^ left
  -> a -- ^ right
  -> a -- ^ bottom
  -> a -- ^ top
  -> a -- ^ near
  -> a -- ^ far
  -> M44 a
inverseFrustum l r b t n f = 
  V4 (V4 rx 0 0 ax)
     (V4 0 ry 0 by)
     (V4 0 0 0 (-1))
     (V4 0 0 rd cd)
  where
    hrn  = 0.5/n
    hrnf = 0.5/(n*f)
    rx = (r-l)*hrn
    ry = (t-b)*hrn
    ax = (r+l)*hrn
    by = (t+b)*hrn
    cd = (f+n)*hrnf
    rd = (n-f)*hrnf

-- | Build a matrix for a symmetric perspective-view frustum with a far plane at infinite
infinitePerspective
  :: Floating a
  => a -- ^ FOV
  -> a -- ^ Aspect Ratio
  -> a -- ^ Near plane
  -> M44 a
infinitePerspective fovy a n =
  V4 (V4 x 0 0    0)
     (V4 0 y 0    0)
     (V4 0 0 (-1) w)
     (V4 0 0 (-1) 0)
  where
    t = n*tan(fovy/2)
    b = -t
    l = b*a
    r = t*a
    x = (2*n)/(r-l)
    y = (2*n)/(t-b)
    w = -2*n

inverseInfinitePerspective
  :: Floating a
  => a -- ^ FOV
  -> a -- ^ Aspect Ratio
  -> a -- ^ Near plane
  -> M44 a
inverseInfinitePerspective fovy a n =
  V4 (V4 ix 0 0  0)
     (V4 0 iy 0  0)
     (V4 0 0  0  (-1))
     (V4 0 0  iw (-iw))
  where
    t = n*tan(fovy/2)
    b = -t
    l = b*a
    r = t*a
    hrn = 0.5/n
    ix = (r-l)*hrn
    iy = (t-b)*hrn
    iw = -hrn

-- | Build an orthographic perspective matrix from 6 clipping planes
ortho
  :: Floating a
  => a -- ^ Left
  -> a -- ^ Right
  -> a -- ^ Bottom
  -> a -- ^ Top
  -> a -- ^ Near
  -> a -- ^ Far
  -> M44 a
ortho left right bottom top near far =
  V4 (V4 (2 / a) 0       0        (negate (right + left) / a))
     (V4 0       (2 / b) 0        (negate (top + bottom) / b))
     (V4 0       0       (-2 / c) (negate (far + near) / c))
     (V4 0       0       0        1)
  where a = right - left
        b = top - bottom
        c = far - near

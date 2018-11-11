{-# LANGUAGE DataKinds #-}
module Invertible (tests) where

import Linear.V
import Linear.Matrix
import qualified Data.Vector as V
import Data.Maybe
import Test.HUnit

v1 :: V 11 Rational
v1 = fromJust (fromVector (V.fromList
  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))

v2 :: V 11 Rational
v2 = fromJust (fromVector (V.fromList
  [8701, 7657, 6616, 5583, 4567, 3583, 2654, 1813, 1105, 589, 340]))

l :: Int -> V 11 Rational
l i = fromJust (fromVector (V.fromList  (
  [1 .. fromIntegral i] ++ [1] ++ replicate (11 - i - 1) 0)))

u :: Int -> V 11 Rational
u i = fromJust (fromVector (V.fromList  (
  replicate i 0 ++ [1] ++ [1 .. fromIntegral (11 - i - 1)])))

ml :: V 11 (V 11 Rational)
ml = fromJust (fromVector (V.fromList [l i | i <- [0 .. 10]]))

mu :: V 11 (V 11 Rational)
mu = fromJust (fromVector (V.fromList [u i | i <- [0 .. 10]]))

m :: V 11 (V 11 Rational)
m = mu !*! ml

tests :: Test
tests = test [ inv identity == (identity :: V 33 (V 33 Rational)) ~?= True
             , inv (inv m) == m ~?= True
             , m !* v1 == v2 ~?= True
             , inv (m) !* v2 == v1 ~?= True
             ]


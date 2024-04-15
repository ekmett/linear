{-# LANGUAGE DataKinds #-}
module Unit.V (tests) where

import Control.DeepSeq (rnf)
import qualified Data.Vector.Unboxed as U (fromList)
import Linear.V (V)
import Test.HUnit

v10 :: V 10 Int
v10 = return 5

tests :: Test
tests = test [ "GH124" ~: rnf (U.fromList [v10]) ~?= () ]

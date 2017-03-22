{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DataKinds #-}
#endif
module V (tests) where

import Test.HUnit

#if __GLASGOW_HASKELL__ >= 707
import Control.DeepSeq (rnf)
import qualified Data.Vector.Unboxed as U (fromList)
import Linear.V (V)

v10 :: V 10 Int
v10 = return 5
#endif

tests :: Test
tests = test [
#if __GLASGOW_HASKELL__ >= 707
               "GH124" ~: rnf (U.fromList [v10]) ~?= ()
#endif
             ]

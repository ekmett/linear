{-# OPTIONS_GHC -Wno-orphans #-}
module Prop.Quaternion (tests) where

import Linear.Quaternion (Quaternion(..))
import Linear.Epsilon (nearZero)
import Linear.Vector (lerp)
import Test.QuickCheck (Arbitrary(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Prop.V3 ()

instance Arbitrary a => Arbitrary (Quaternion a) where
  arbitrary = Quaternion <$> arbitrary <*> arbitrary

prop_lerp0 :: Quaternion Double -> Quaternion Double -> Bool
prop_lerp0 a b = nearZero (lerp 0 a b - a)

prop_lerp1 :: Quaternion Double -> Quaternion Double -> Bool
prop_lerp1 a b = nearZero (lerp 1 a b - b)

tests :: [TestTree]
tests =
  [ testGroup "lerp"
    [ testProperty "lerp 0 a b == a" prop_lerp0
    , testProperty "lerp 1 a b == b" prop_lerp1
    ]
  ]

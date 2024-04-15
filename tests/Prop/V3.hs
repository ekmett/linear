{-# OPTIONS_GHC -Wno-orphans #-}
module Prop.V3 () where

import Linear.V3 (V3(..))
import Test.QuickCheck (Arbitrary(..))

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

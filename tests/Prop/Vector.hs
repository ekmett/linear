{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Prop.Vector (tests) where

import Linear.V0 (V0)
import Linear.V1 (V1)
import Linear.V2 (V2)
import Linear.V3 (V3)
import Linear.V4 (V4)
import Linear.Vector (Additive (..), negated, zero, (*^), (^*))
import Prop.V0 ()
import Prop.V1 ()
import Prop.V2 ()
import Prop.V3 ()
import Prop.V4 ()
import Prop.V (prop1_V, prop2_V, prop3_V)
import Test.QuickCheck (Property, (.&&.))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

#define ALLVECTORS(testname) \
  testname @V0 @Rational .&&. \
  testname @V1 @Rational .&&. \
  testname @V2 @Rational .&&. \
  testname @V3 @Rational .&&. \
  testname @V4 @Rational

prop_addAssoc :: Property
prop_addAssoc = ALLVECTORS(prop) .&&. prop3_V @Rational prop
 where
  prop :: (Eq (v a), Additive v, Num a) => v a -> v a -> v a -> Bool
  prop a b c = ((a ^+^ b) ^+^ c) == (a ^+^ (b ^+^ c))

prop_addCommut :: Property
prop_addCommut = ALLVECTORS(prop) .&&. prop2_V @Rational prop
 where
  prop :: (Eq (v a), Additive v, Num a) => v a -> v a -> Bool
  prop a b = (a ^+^ b) == (b ^+^ a)

prop_LRScalarProduct :: Property
prop_LRScalarProduct = ALLVECTORS(prop) .&&. prop1_V @Rational prop
 where
  prop :: (Eq (v a), Functor v, Num a) => v a -> a -> Bool
  prop v a = v ^* a == a *^ v

prop_distScalarR :: Property
prop_distScalarR = ALLVECTORS(prop) .&&. prop2_V @Rational prop
 where
  prop :: (Eq (v a), Additive v, Num a) => v a -> v a -> a -> Bool
  prop a b c = (a ^+^ b) ^* c == (a ^* c) ^+^ (b ^* c)

prop_distScalarL :: Property
prop_distScalarL = ALLVECTORS(prop) .&&. prop2_V @Rational prop
 where
  prop :: (Eq (v a), Additive v, Num a) => v a -> v a -> a -> Bool
  prop a b c = c *^ (a ^+^ b) == (c *^ a) ^+^ (c *^ b)

prop_negateVector :: Property
prop_negateVector = ALLVECTORS(prop) .&&. prop1_V @Rational prop
 where
  prop :: (Eq (v a), Additive v, Num a) => v a -> Bool
  prop a = (a ^+^ negated a) == zero

tests :: [TestTree]
tests =
  [ testProperty "left and right scalar product are equal" prop_LRScalarProduct
  , testProperty "right scalar mult is distributive over vector addition" prop_distScalarR
  , testProperty "left scalar mult is distributive over vector addition" prop_distScalarL
  , testProperty "negation is inverse under ^+^" prop_negateVector
  , testProperty "associativity of ^+^" prop_addAssoc
  , testProperty "commutativity of ^+^" prop_addCommut
  ]

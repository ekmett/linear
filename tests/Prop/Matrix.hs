{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Prop.Matrix (tests) where

import Linear.Matrix (
  Trace (trace),
  det22, inv22,
  det33, inv33,
  det44, inv44,
  identity,
  transpose,
  (!!*),
  (!*!),
  (!+!),
  (*!!),
 )
import Linear.V2 (V2)
import Linear.V3 (V3)
import Linear.V4 (V4)
import Linear.V (V)
import Linear.Vector (Additive)
import Prop.V2 ()
import Prop.V3 ()
import Prop.V4 ()
import Prop.V (prop1_V, prop2_V, prop3_V)
import Test.QuickCheck (Property, (.&&.), (==>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Data.Distributive (Distributive)

#define SQUAREMATRIX(testname) \
  testname @V2 @Rational .&&. \
  testname @V3 @Rational .&&. \
  testname @V4 @Rational

#define ALLMATRIX(testname) \
  testname @V2 @V2 @Rational .&&. \
  testname @V2 @V3 @Rational .&&. \
  testname @V2 @V4 @Rational .&&. \
  testname @V3 @V2 @Rational .&&. \
  testname @V3 @V3 @Rational .&&. \
  testname @V3 @V3 @Rational .&&. \
  testname @V4 @V2 @Rational .&&. \
  testname @V4 @V3 @Rational .&&. \
  testname @V4 @V4 @Rational .&&. \
  testname @V4 @(V 7) @Rational .&&. \
  testname @(V 10) @(V 6) @Rational .&&. \
  testname @(V 20) @(V 15) @Rational

#define VMATRIX1(testname) \
  prop1_V @(V2 Rational) testname .&&. \
  prop1_V @(V3 Rational) testname .&&. \
  prop1_V @(V4 Rational) testname .&&. \
  prop1_V @(V 15 Rational) testname

#define VMATRIX2(testname) \
  prop2_V @(V2 Rational) testname .&&. \
  prop2_V @(V3 Rational) testname .&&. \
  prop2_V @(V4 Rational) testname .&&. \
  prop2_V @(V 10 Rational) testname

#define VMATRIX3(testname) \
  prop3_V @(V2 Rational) testname .&&. \
  prop3_V @(V3 Rational) testname .&&. \
  prop3_V @(V4 Rational) testname .&&. \
  prop3_V @(V 8 Rational) testname

class SquareMatrix m where
  det :: Num a => m (m a) -> a
  inv :: Fractional a => m (m a) -> m (m a)

instance SquareMatrix V2 where
  det = det22
  inv = inv22

instance SquareMatrix V3 where
  det = det33
  inv = inv33

instance SquareMatrix V4 where
  det = det44
  inv = inv44

-- Properties of general matrices
prop_addCommut :: Property
prop_addCommut = ALLMATRIX(prop) .&&. VMATRIX2(prop)
 where
  prop :: (Eq (m (n a)), Additive m, Additive n, Num a) => m (n a) -> m (n a) -> Bool
  prop a b = (a !+! b) == (b !+! a)

prop_addAssoc :: Property
prop_addAssoc = ALLMATRIX(prop) .&&. VMATRIX3(prop)
 where
  prop :: (Eq (m (n a)), Additive m, Additive n, Num a) => m (n a) -> m (n a) -> m (n a) -> Bool
  prop a b c = ((a !+! b) !+! c) == (a !+! (b !+! c))

prop_distOfScalar :: Property
prop_distOfScalar = ALLMATRIX(prop) .&&. VMATRIX2(prop)
 where
  prop :: (Eq (m (n a)), Additive m, Additive n, Foldable m, Num a)
    => m (n a) -> m (n a) -> a -> Bool
  prop b c a = a *!! (b !+! c) == ((a *!! b) !+! (a *!! c))

prop_LRScalar :: Property
prop_LRScalar = ALLMATRIX(prop) .&&. VMATRIX1(prop)
 where
  prop :: (Functor m, Functor n, Num a, Eq (m (n a))) => m (n a) -> a -> Bool
  prop m a = m !!* a == a *!! m

-- Transpose properties
prop_transpose :: Property
prop_transpose = ALLMATRIX(prop) .&&. VMATRIX1(prop)
 where
  prop :: (Eq (m (n a)), Distributive m, Distributive n) => m (n a) -> Bool
  prop a = transpose (transpose a) == a

prop_transposeDistAdd :: Property
prop_transposeDistAdd = ALLMATRIX(prop) .&&. VMATRIX2(prop)
 where
  prop :: ( Eq (n (m a)), Additive m, Distributive m, Distributive n,
    Additive n , Num a)
    => m (n a) -> m (n a) -> Bool
  prop a b = transpose (a !+! b) == (transpose a !+! transpose b)

prop_transposeDistMul :: Property
prop_transposeDistMul = ALLMATRIX(prop)
 where
  prop :: ( Additive m, Foldable m, Distributive m, Distributive n, Foldable n
    , Additive n, Num a, Eq (m (m a)))
    => m (n a) -> n (m a) -> Bool
  prop a b = transpose (a !*! b) == (transpose b !*! transpose a)

-- Identity properties
prop_identityNeutralL :: Property
prop_identityNeutralL = ALLMATRIX(prop) .&&. VMATRIX1(prop)
 where
  prop :: ( Eq (m (n a)), Functor m, Additive n, Traversable n, Applicative n, Num a)
    => m (n a) -> Bool
  prop a = a !*! identity == a

prop_identityNeutralR :: Property
prop_identityNeutralR = ALLMATRIX(prop) .&&. VMATRIX1(prop)
 where
  prop :: ( Eq (m (n a)), Additive m, Foldable m, Traversable m,
    Applicative m, Additive n, Num a)
    => m (n a) -> Bool
  prop a = identity !*! a == a

-- Properties of square matrices
prop_mulAssoc :: Property
prop_mulAssoc = SQUAREMATRIX(prop)
 where
  prop :: (Eq (m (m a)), Additive m, Foldable m, Num a)
    => m (m a) -> m (m a) -> m (m a) -> Bool
  prop a b c = ((a !*! b) !*! c) == (a !*! (b !*! c))

prop_distOfMatrix :: Property
prop_distOfMatrix = SQUAREMATRIX(prop)
 where
  prop :: (Eq (m (m a)), Additive m, Foldable m, Num a)
    => m (m a) -> m (m a) -> m (m a) -> Bool
  prop a b c = (a !*! (b !+! c)) == ((a !*! b) !+! (a !*! c))

-- Inverse properties
prop_inv :: Property
prop_inv = SQUAREMATRIX(prop)
 where
  prop :: (Additive m, Fractional a, SquareMatrix m, Eq (m (m a)), Eq a)
    => m (m a) -> Property
  prop a = (det a /= 0) ==> inv (inv a) == a

prop_invIdent :: Property
prop_invIdent = SQUAREMATRIX(prop)
 where
  prop :: ( Additive m, Foldable m, Traversable m, Applicative m, Fractional a
    , SquareMatrix m, Eq (m (m a)), Eq a )
    => m (m a) -> Property
  prop a = det a /= 0 ==> a !*! inv a == identity

prop_invMult :: Property
prop_invMult = SQUAREMATRIX(prop)
 where
  prop :: ( Additive m, Foldable m, Fractional a, SquareMatrix m, Eq a, Eq (m (m a)))
    => m (m a) -> m (m a) -> Property
  prop a b = det a /= 0 && det b /= 0 ==> (inv (a !*! b) == (inv b !*! inv a))

-- Determinant properties
prop_detTranspose :: Property
prop_detTranspose = SQUAREMATRIX(prop)
 where
  prop :: (Additive m, Distributive m, Fractional a, SquareMatrix m, Eq a)
    => m (m a) -> Bool
  prop a = det (transpose a) == det a

prop_detProd :: Property
prop_detProd = SQUAREMATRIX(prop)
 where
  prop :: (Additive m, Foldable m, Fractional a, SquareMatrix m, Eq a)
    => m (m a) -> m (m a) -> Bool
  prop a b = det (a !*! b) == det a * det b

prop_detScalarPow :: Property
prop_detScalarPow = SQUAREMATRIX(prop)
 where
  prop :: (Additive m, Fractional a, Foldable m, SquareMatrix m, Eq a)
    => m (m a) -> a -> Bool
  prop a c = det (c *!! a) == (c ^ n) * det a
   where
    n = length a

-- Trace properties
prop_traceLinear :: Property
prop_traceLinear = SQUAREMATRIX(prop) .&&. prop @(V 10) @Rational
 where
  prop :: (Trace m, Additive m, Num a, Eq a) => m (m a) -> m (m a) -> Bool
  prop a b = trace (a !+! b) == (trace a + trace b)

prop_traceTranspose :: Property
prop_traceTranspose = SQUAREMATRIX(prop) .&&. prop @(V 10) @Rational
 where
  prop :: (Trace m, Distributive m, Num a, Eq a) => m (m a) -> Bool
  prop a = trace a == trace (transpose a)

prop_traceSwap :: Property
prop_traceSwap = SQUAREMATRIX(prop) .&&. prop @(V 15) @Rational
 where
  prop :: (Foldable m, Trace m, Additive m, Eq a, Num a)
    => m (m a) -> m (m a) -> Bool
  prop a b = trace (a !*! b) == trace (b !*! a)

tests :: [TestTree]
tests =
  [ testGroup
      "General Matrix Properties"
      [ testGroup
          "Basic Properties"
          [ testProperty "commutativity of !+!" prop_addCommut
          , testProperty "associativity of !+!" prop_addAssoc
          , testProperty "distributivity of scalar mult" prop_distOfScalar
          , testProperty "left and right scalar product are equal" prop_LRScalar
          ]
      , testGroup
          "Transpose Properties"
          [ testProperty "(A^T)^T == A" prop_transpose
          , testProperty "(A !+! B)^T == (A^T !+! B^T)" prop_transposeDistAdd
          , testProperty "(AB)^T == (B^T !*! A^T)" prop_transposeDistMul
          ]
      , testGroup
          "Identity Properties"
          [ testProperty "identity is neutral under !*!" prop_identityNeutralR
          , testProperty "identity is neutral under !*!" prop_identityNeutralL
          ]
      ]
  , testGroup
      "Square matrix properties"
      [ testProperty "associativity of !*!" prop_mulAssoc
      , testProperty "distributivity of !*!" prop_distOfMatrix
      , testGroup "Inverse properties"
        [ testProperty "inv (inv a) == a" prop_inv
        , testProperty "a !*! inv a == I" prop_invIdent
        , testProperty "(AB)^-1 == B^-1 * A^-1" prop_invMult
        ]
      , testGroup "Determinant properties"
        [ testProperty "det A^T = det A" prop_detTranspose
        , testProperty "det (AB) = det A * det B" prop_detProd
        , testProperty "det (cA) = c^2 * det A" prop_detScalarPow
        ]
      , testGroup
        "Trace Properties"
        [ testProperty "trace (A+B) == trace A + trace B" prop_traceLinear
        , testProperty "trace A == trace (A^T)" prop_traceTranspose
        , testProperty "trace (AB) == trace (BA)" prop_traceSwap
        ]
      ]
  ]

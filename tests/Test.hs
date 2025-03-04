{-# LANGUAGE CPP #-}
module Main (main) where

import Test.Tasty (defaultMain, testGroup, TestTree)

import qualified Prop.Quaternion
import qualified Unit.Binary
import qualified Unit.Plucker
import qualified Unit.V

tests :: [TestTree]
tests =
  [ testGroup "Property tests"
    [ testGroup "Quaternion" Prop.Quaternion.tests
    ]
  , testGroup "Unit tests"
    [ testGroup "Binary" Unit.Binary.tests
    , testGroup "Plucker" Unit.Plucker.tests
    , testGroup "V" Unit.V.tests
    ]
  ]

main :: IO ()
main = defaultMain $ testGroup "linear" tests

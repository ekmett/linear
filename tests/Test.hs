{-# LANGUAGE CPP #-}
module Main (main) where
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import qualified Unit.Binary
import qualified Unit.Plucker
import qualified Unit.V

tests :: [Test]
tests =
  [ testGroup "Unit tests"
    [ testGroup "Binary" $ hUnitTestToTests Unit.Binary.tests
    , testGroup "Plucker" $ hUnitTestToTests Unit.Plucker.tests
    , testGroup "V" $ hUnitTestToTests Unit.V.tests
    ]
  ]

main :: IO ()
main = defaultMain tests

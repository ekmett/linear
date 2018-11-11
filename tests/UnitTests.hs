{-# LANGUAGE CPP #-}
module Main (main) where
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import qualified Plucker
import qualified Binary
import qualified Invertible
#if __GLASGOW_HASKELL__ >= 707
import qualified V
#endif

tests :: [Test]
tests = [ testGroup "Plucker" $ hUnitTestToTests Plucker.tests
        , testGroup "Binary" $ hUnitTestToTests Binary.tests
        , testGroup "Invertible" $ hUnitTestToTests Invertible.tests
#if __GLASGOW_HASKELL__ >= 707
        , testGroup "V" $ hUnitTestToTests V.tests
#endif
        ]

main :: IO ()
main = defaultMain tests

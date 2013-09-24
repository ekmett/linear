module Main (main) where
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import qualified Plucker
import qualified Binary

tests :: [Test]
tests = [ testGroup "Plucker" $ hUnitTestToTests Plucker.tests 
        , testGroup "Binary" $ hUnitTestToTests Binary.tests ]

main :: IO ()
main = defaultMain tests

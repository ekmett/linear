module Main (main) where
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import qualified Plucker

tests :: [Test]
tests = [ testGroup "Plucker" $ hUnitTestToTests Plucker.tests ]

main :: IO ()
main = defaultMain tests

module Main (main) where
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import qualified Plucker
import qualified Binary
import qualified V

tests :: [Test]
tests = [ testGroup "Plucker" $ hUnitTestToTests Plucker.tests
        , testGroup "Binary" $ hUnitTestToTests Binary.tests
        , testGroup "V" $ hUnitTestToTests V.tests ]

main :: IO ()
main = defaultMain tests

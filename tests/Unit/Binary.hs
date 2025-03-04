module Unit.Binary (tests) where

import Data.Binary.Put
import Data.Binary.Get
import Linear
import qualified Data.ByteString.Lazy as BS
import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@?=), testCase)

originalVecs :: (V3 Float, V2 Char)
originalVecs = (V3 1 2 3, V2 'a' 'b')

bytes :: BS.ByteString
bytes = runPut $ do putLinear $ fst originalVecs
                    putLinear $ snd originalVecs

tests :: [TestTree]
tests = [ testCase "Serialized length" $ BS.length bytes @?= 3*13+2
        , testCase "Deserialization" $ deserialized @?= originalVecs ]
  where deserialized = runGet ((,) <$> getLinear <*> getLinear) bytes

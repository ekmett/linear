module Binary (tests) where

import Data.Binary.Put
import Data.Binary.Get
import Linear
import qualified Data.ByteString.Lazy as BS
import Test.HUnit

originalVecs :: (V3 Float, V2 Char)
originalVecs = (V3 1 2 3, V2 'a' 'b')

bytes :: BS.ByteString
bytes = runPut $ do putLinear $ fst originalVecs
                    putLinear $ snd originalVecs

tests :: Test
tests = test [ "Serialized length" ~: BS.length bytes ~?= 3*13+2
             , "Deserialization" ~: deserialized ~?= originalVecs ]
  where deserialized = runGet ((,) <$> getLinear <*> getLinear) bytes

module Test.Network.Wire (tests) where

import qualified Data.ByteString as BS
import Data.Word (Word64)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Network.P2P.Wire (encodeWire, decodeWire, compressWire, decompressWire)

tests :: TestTree
tests = testGroup "Network.P2P.Wire"
  [ testCase "compression roundtrip for empty bytestring" $ do
      let bs = BS.empty
      decompressWire (compressWire bs) @?= bs

  , testCase "compression roundtrip for known data" $ do
      let bs = BS.replicate 256 0xAB
      decompressWire (compressWire bs) @?= bs

  , testProperty "compression roundtrip for arbitrary bytes" $
      \ws -> let bs = BS.pack ws
             in  decompressWire (compressWire bs) == bs

  , testProperty "wire roundtrip for Word64" $
      \(w :: Word64) ->
        decodeWire (encodeWire w) == Right w

  , testCase "compressed data is smaller for repetitive input" $ do
      let largeRepetitive = BS.replicate 1000 0x42
          compressed = compressWire largeRepetitive
      assertBool "compressed should be smaller than original for repetitive data"
        (BS.length compressed < BS.length largeRepetitive)
  ]

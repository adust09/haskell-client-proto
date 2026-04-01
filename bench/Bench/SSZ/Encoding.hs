-- | Benchmarks for SSZ serialization throughput (encode/decode).
module Bench.SSZ.Encoding (benchmarks) where

import Test.Tasty.Bench
import Data.ByteString (ByteString)
import Data.Word (Word64)

import SSZ.Common (SszEncode (..), SszDecode (..))
import Consensus.Types
import Bench.Support.Generators

benchmarks :: Benchmark
benchmarks = bgroup "SSZ.Encoding"
  [ bgroup "encode"
      [ bench "Word64"            $ nf sszEncode (42 :: Word64)
      , bench "Checkpoint"        $ nf sszEncode sampleCheckpoint
      , bench "BeaconBlockHeader" $ nf sszEncode sampleHeader
      , bench "BeaconBlock"       $ nf sszEncode sampleBlock4
      ]
  , bgroup "decode"
      [ bench "Word64"            $ nf (sszDecode @Word64) encodedWord64
      , bench "Checkpoint"        $ nf (sszDecode @Checkpoint) encodedCheckpoint
      , bench "BeaconBlockHeader" $ nf (sszDecode @BeaconBlockHeader) encodedHeader
      ]
  , bgroup "roundtrip"
      [ bench "BeaconBlockHeader" $
          nf (\h -> sszDecode @BeaconBlockHeader (sszEncode h)) sampleHeader
      ]
  ]

-- Pre-encoded data for decode benchmarks
encodedWord64 :: ByteString
encodedWord64 = sszEncode (42 :: Word64)

encodedCheckpoint :: ByteString
encodedCheckpoint = sszEncode sampleCheckpoint

encodedHeader :: ByteString
encodedHeader = sszEncode sampleHeader

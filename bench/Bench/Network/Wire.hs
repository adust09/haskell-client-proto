-- | Benchmarks for wire format (SSZ + zlib compression).
module Bench.Network.Wire (benchmarks) where

import Test.Tasty.Bench

import Consensus.Types (BeaconBlock)
import Network.P2P.Wire (encodeWire, decodeWire, compressWire, decompressWire)
import Bench.Support.Generators

benchmarks :: Benchmark
benchmarks = bgroup "Network.P2P.Wire"
  [ bgroup "compress"
      [ bench "BeaconBlock" $ nf compressWire encodedBlock
      ]
  , bgroup "decompress"
      [ bench "BeaconBlock" $ nf decompressWire compressedBlock
      ]
  , bgroup "encodeWire"
      [ bench "BeaconBlock" $ nf encodeWire sampleBlock4
      ]
  , bgroup "decodeWire"
      [ bench "BeaconBlock" $ nf (decodeWire @BeaconBlock) compressedBlock
      ]
  ]

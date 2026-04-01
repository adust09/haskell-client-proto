-- | Benchmarks for SHA-256 hashing primitives.
-- These establish the theoretical throughput floor for merkleization.
module Bench.Crypto.Hashing (benchmarks) where

import Test.Tasty.Bench

import Crypto.Hashing (sha256, sha256Pair)
import Bench.Support.Generators (chunk32, chunk32b, bs64, bs1024)

benchmarks :: Benchmark
benchmarks = bgroup "Crypto.Hashing"
  [ bgroup "sha256"
      [ bench "32 bytes"   $ nf sha256 chunk32
      , bench "64 bytes"   $ nf sha256 bs64
      , bench "1024 bytes" $ nf sha256 bs1024
      ]
  , bench "sha256Pair" $ nf (uncurry sha256Pair) (chunk32, chunk32b)
  ]

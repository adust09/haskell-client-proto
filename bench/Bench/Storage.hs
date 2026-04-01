-- | Benchmarks for RocksDB storage operations.
module Bench.Storage (benchmarks) where

import Test.Tasty.Bench

import Storage (putBlock, getBlock, putState, getState)
import Bench.Support.Generators

benchmarks :: Benchmark
benchmarks = bgroup "Storage"
  [ bench "putBlock" $
      nfIO (withBenchStorage $ \sh -> putBlock sh sampleRoot sampleSignedBlock4)
  , bench "getBlock/hit" $
      nfIO (withBenchStorage $ \sh -> do
        putBlock sh sampleRoot sampleSignedBlock4
        getBlock sh sampleRoot)
  , bench "getBlock/miss" $
      nfIO (withBenchStorage $ \sh -> getBlock sh sampleRoot)
  , bench "putState" $
      nfIO (withBenchStorage $ \sh -> putState sh sampleRoot genesisState4)
  , bench "getState" $
      nfIO (withBenchStorage $ \sh -> do
        putState sh sampleRoot genesisState4
        getState sh sampleRoot)
  ]

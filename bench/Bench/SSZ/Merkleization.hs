-- | Benchmarks for SSZ merkleization — the hottest path in consensus.
-- hash_tree_root is called by every state transition and fork choice operation.
module Bench.SSZ.Merkleization (benchmarks) where

import Test.Tasty.Bench

import SSZ.Merkleization (merkleize, pack, SszHashTreeRoot (..))
import Bench.Support.Generators

benchmarks :: Benchmark
benchmarks = bgroup "SSZ.Merkleization"
  [ bgroup "merkleize"
      [ bench "1 chunk/limit=1"     $ nf (uncurry merkleize) ([chunk32], 1)
      , bench "4 chunks/limit=4"    $ nf (uncurry merkleize) (chunks4, 4)
      , bench "64 chunks/limit=64"  $ nf (uncurry merkleize) (chunks64, 64)
      , bench "256 chunks/limit=256" $ nf (uncurry merkleize) (chunks256, 256)
      ]
  , bgroup "pack"
      [ bench "32 bytes"   $ nf (pack . pure) chunk32
      , bench "1024 bytes" $ nf (pack . pure) bs1024
      ]
  , bgroup "hashTreeRoot"
      [ bench "BeaconBlockHeader"    $ nf hashTreeRoot sampleHeader
      , bench "BeaconBlock"          $ nf hashTreeRoot sampleBlock4
      , bench "Checkpoint"           $ nf hashTreeRoot sampleCheckpoint
      , bench "BeaconState/4-vals"   $ nf hashTreeRoot genesisState4
      , bench "BeaconState/128-vals" $ nf hashTreeRoot genesisState128
      ]
  ]

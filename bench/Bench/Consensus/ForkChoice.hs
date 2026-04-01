-- | Benchmarks for fork choice — head selection scales with chain length.
module Bench.Consensus.ForkChoice (benchmarks) where

import Test.Tasty.Bench

import Consensus.ForkChoice (onBlock, getWeight, getAncestor)
import Bench.Support.Generators

benchmarks :: Benchmark
benchmarks = bgroup "Consensus.ForkChoice"
  [ bgroup "onBlock"
      [ bench "5-block chain"  $ nf (uncurry onBlock) (fst chainStore5, nextBlock5)
      , bench "50-block chain" $ nf (uncurry onBlock) (fst chainStore50, nextBlock50)
      ]
  , bgroup "getWeight"
      [ bench "5-block chain"  $ nf (uncurry getWeight) (fst chainStore5, headRoot5)
      , bench "50-block chain" $ nf (uncurry getWeight) (fst chainStore50, headRoot50)
      ]
  , bgroup "getAncestor"
      [ bench "depth 5/slot 0"   $
          nf (\r -> getAncestor (fst chainStore5) r 0) headRoot5
      , bench "depth 50/slot 0"  $
          nf (\r -> getAncestor (fst chainStore50) r 0) headRoot50
      , bench "depth 50/slot 45" $
          nf (\r -> getAncestor (fst chainStore50) r 45) headRoot50
      ]
  ]

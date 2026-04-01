-- | Benchmarks for consensus state transition — the critical throughput metric.
-- stateTransition must complete within the 4-second slot window minus network delay.
module Bench.Consensus.StateTransition (benchmarks) where

import Test.Tasty.Bench

import Consensus.StateTransition (processSlot, processSlots, stateTransition)
import Bench.Support.Generators

benchmarks :: Benchmark
benchmarks = bgroup "Consensus.StateTransition"
  [ bgroup "processSlot"
      [ bench "single slot" $ nf processSlot genesisState4
      ]
  , bgroup "processSlots"
      [ bench "advance 1 slot"    $ nf (processSlots genesisState4) 1
      , bench "advance 10 slots"  $ nf (processSlots genesisState4) 10
      , bench "advance 100 slots" $ nf (processSlots genesisState4) 100
      ]
  , bgroup "stateTransition"
      [ bench "empty block/4 validators"   $
          nf (\sb -> stateTransition genesisState4 sb False) sampleSignedBlock4
      , bench "empty block/128 validators" $
          nf (\sb -> stateTransition genesisState128 sb False) sampleSignedBlock128
      , bench "empty block/4096 validators" $
          nf (\sb -> stateTransition genesisState4096 sb False) sampleSignedBlock4096
      ]
  ]

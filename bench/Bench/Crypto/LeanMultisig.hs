-- | Benchmarks for leanMultisig aggregation (currently stub).
module Bench.Crypto.LeanMultisig (benchmarks) where

import Test.Tasty.Bench

import Crypto.LeanMultisig (ProverContext (..), VerifierContext (..), aggregate, verifyAggregation)
import Bench.Support.Generators

benchmarks :: Benchmark
benchmarks = bgroup "Crypto.LeanMultisig"
  [ bench "aggregate/3 signers"   $
      nfIO (aggregate ProverContext testSigners3 testMessage)
  , bench "verifyAggregation/3"   $
      nfIO (verifyAggregation VerifierContext testAggProof3 (map fst testSigners3) testMessage)
  ]

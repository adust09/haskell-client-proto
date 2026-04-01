-- | Benchmarks for XMSS signature operations (currently Ed25519 stub).
module Bench.Crypto.LeanSig (benchmarks) where

import Test.Tasty.Bench

import Crypto.LeanSig (generateKeyPair, sign, verify)
import Bench.Support.Generators

benchmarks :: Benchmark
benchmarks = bgroup "Crypto.LeanSig"
  [ bench "generateKeyPair" $ nf (generateKeyPair 10) "bench-seed"
  , bench "sign"            $ nf (\msg -> sign testPrivKey msg 0) testMessage
  , bench "verify"          $ nf (\sig -> verify testPubKey testMessage sig) testSignature
  ]

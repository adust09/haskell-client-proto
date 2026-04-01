module Main (main) where

import Test.Tasty.Bench

import qualified Bench.Crypto.Hashing as Hashing
import qualified Bench.SSZ.Merkleization as Merkleization
import qualified Bench.SSZ.Encoding as Encoding
import qualified Bench.Consensus.StateTransition as StateTransition
import qualified Bench.Consensus.ForkChoice as ForkChoice

main :: IO ()
main = defaultMain
  [ Hashing.benchmarks
  , Merkleization.benchmarks
  , Encoding.benchmarks
  , StateTransition.benchmarks
  , ForkChoice.benchmarks
  ]

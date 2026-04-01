module Main (main) where

import Test.Tasty.Bench

import qualified Bench.Crypto.Hashing as Hashing
import qualified Bench.Crypto.LeanSig as LeanSig
import qualified Bench.Crypto.LeanMultisig as LeanMultisig
import qualified Bench.SSZ.Merkleization as Merkleization
import qualified Bench.SSZ.Encoding as Encoding
import qualified Bench.Consensus.StateTransition as StateTransition
import qualified Bench.Consensus.ForkChoice as ForkChoice
import qualified Bench.Network.Wire as Wire
import qualified Bench.Storage as Storage

main :: IO ()
main = defaultMain
  [ Hashing.benchmarks
  , LeanSig.benchmarks
  , LeanMultisig.benchmarks
  , Merkleization.benchmarks
  , Encoding.benchmarks
  , StateTransition.benchmarks
  , ForkChoice.benchmarks
  , Wire.benchmarks
  , Storage.benchmarks
  ]

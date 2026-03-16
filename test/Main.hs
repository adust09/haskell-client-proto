module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import LeanConsensus (version)
import qualified Test.Consensus.Types as ConsensusTypes
import qualified Test.SSZ.Bitlist as SSZBitlist
import qualified Test.SSZ.Bitvector as SSZBitvector
import qualified Test.SSZ.Common as SSZCommon
import qualified Test.SSZ.Derive as SSZDerive
import qualified Test.SSZ.List as SSZList
import qualified Test.SSZ.Merkleization as SSZMerkleization
import qualified Test.SSZ.Vector as SSZVector
import qualified Test.Crypto.Hashing as CryptoHashing
import qualified Test.Crypto.LeanSig as CryptoLeanSig
import qualified Test.Crypto.KeyManager as CryptoKeyManager
import qualified Test.Crypto.LeanMultisig as CryptoLeanMultisig
import qualified Test.Crypto.Operations as CryptoOperations

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "lean-consensus"
  [ testCase "version is not empty" $
      assertBool "version should not be empty" (not $ null version)
  , SSZCommon.tests
  , SSZList.tests
  , SSZVector.tests
  , SSZBitvector.tests
  , SSZBitlist.tests
  , SSZMerkleization.tests
  , SSZDerive.tests
  , ConsensusTypes.tests
  , CryptoHashing.tests
  , CryptoLeanSig.tests
  , CryptoKeyManager.tests
  , CryptoLeanMultisig.tests
  , CryptoOperations.tests
  ]

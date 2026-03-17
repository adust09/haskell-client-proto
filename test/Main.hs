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
import qualified Test.Consensus.SlotTimer as SlotTimer
import qualified Test.Consensus.StateTransition as StateTransition
import qualified Test.Consensus.ForkChoice as ForkChoice
import qualified Test.Consensus.Integration as Integration
import qualified Test.Network.P2PTypes as P2PTypes
import qualified Test.Network.Wire as Wire
import qualified Test.Network.MessageHandler as MessageHandler
import qualified Test.Network.Sync as Sync
import qualified Test.Network.Aggregator as Aggregator
import qualified Test.Network.Integration as NetworkIntegration
import qualified Test.Storage as Storage
import qualified Test.Genesis as Genesis
import qualified Test.Actor as Actor
import qualified Test.Node as Node
import qualified Test.Network.RPC as RPC
import qualified Test.Metrics as Metrics

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
  , SlotTimer.tests
  , StateTransition.tests
  , ForkChoice.tests
  , Integration.tests
  , P2PTypes.tests
  , Wire.tests
  , MessageHandler.tests
  , Sync.tests
  , Aggregator.tests
  , NetworkIntegration.tests
  , Storage.tests
  , Genesis.tests
  , Actor.tests
  , Node.tests
  , RPC.tests
  , Metrics.tests
  ]

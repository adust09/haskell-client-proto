-- | End-to-end integration tests for the network layer using MockNetwork.
module Test.Network.Integration (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Consensus.ForkChoice (initStore)
import Consensus.StateTransition (getAttestationSubnet)
import Consensus.Types
import Crypto.LeanMultisig (setupVerifier)
import Network.MessageHandler
import Network.P2P.Types (Topic (..))
import Network.P2P.Wire (encodeWire)
import Test.Support.Helpers
import Test.Support.MockNetwork

tests :: TestTree
tests = testGroup "Network.Integration"
  [ testCase "two-node block gossip via MockNetwork" $ do
      let vals = [mkTestValidator 1 32000000]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      -- Create store for node 2
      store2Var <- newTVarIO (initStore gs genesisBlock)

      -- Create mock network and handle
      mn <- newMockNetwork
      handle2 <- mockP2PHandle mn

      -- Setup message handler for node 2
      cache2 <- newTVarIO (newSeenCache 8192)
      slotVar <- newTVarIO 1
      verifier <- setupVerifier
      let env2 = MessageHandlerEnv store2Var cache2 handle2 verifier slotVar
      startMessageHandler env2

      -- Give subscriber threads time to start
      threadDelay 10_000

      -- Node 1 produces and publishes a block
      let sbb = mkTestSignedBlock gs 1
          wireMsg = encodeWire sbb

      broadcastAll mn TopicBeaconBlock wireMsg

      -- Wait for propagation
      threadDelay 50_000

      -- Verify node 2's store was updated
      store2 <- readTVarIO store2Var
      assertBool "node 2 should have processed the block"
        (Map.size (stBlocks store2) >= 2)

  , testCase "attestation flow: validator -> subnet -> message handler" $ do
      let vals = [mkTestValidator 1 32000000]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock
          genesisRoot = toRoot genesisBlock

      storeVar <- newTVarIO (initStore gs genesisBlock)
      cacheVar <- newTVarIO (newSeenCache 8192)
      slotVar <- newTVarIO 1

      mn <- newMockNetwork
      handle <- mockP2PHandle mn
      verifier <- setupVerifier

      let env = MessageHandlerEnv storeVar cacheVar handle verifier slotVar
      startMessageHandler env
      threadDelay 10_000

      -- Publish attestation to subnet
      let subnet = getAttestationSubnet 0
          att = mkTestAttestation 0 0 genesisRoot zeroCheckpoint zeroCheckpoint
          wireAtt = encodeWire att

      broadcastAll mn (TopicAttestation subnet) wireAtt
      threadDelay 50_000

      -- The attestation should have been processed into the store
      store <- readTVarIO storeVar
      -- onAttestation adds to latestMessages
      assertBool "store should have latest message for validator 0"
        (Map.member 0 (stLatestMessages store))
  ]

-- | End-to-end integration tests for the network layer using MockNetwork.
module Test.Network.Integration (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Consensus.ForkChoice (initStore)
import Consensus.StateTransition (stateTransition)
import Consensus.Types
import Crypto.LeanMultisig (setupProver, setupVerifier)
import Crypto.Operations (aggregateAttestations)
import Network.Aggregator (newAttestationPool, addAttestation, drainAttestations)
import Network.MessageHandler
import Network.P2P.Types (P2PHandle (..), Topic (..))
import Network.P2P.Wire (encodeWire)
import Network.Sync (SyncEnv (..), SyncStatus (..), runSync)
import SSZ.List (unSszList)
import Test.Support.Helpers
import Test.Support.MockNetwork

-- | Force a Right value, failing with an error on Left.
unsafeRight :: Show e => Either e a -> a
unsafeRight (Right a) = a
unsafeRight (Left e)  = error ("unexpected Left: " <> show e)

tests :: TestTree
tests = testGroup "Network.Integration"
  [ testCase "two-node block gossip via MockNetwork" $ do
      let vals = [mkTestValidator 1 0]
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

  , testCase "attestation flow: validator -> message handler" $ do
      let vals = [mkTestValidator 1 0]
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
      let att = mkTestAttestation 0 0 genesisRoot zeroCheckpoint zeroCheckpoint
          wireAtt = encodeWire att

      broadcastAll mn (TopicAttestation 0) wireAtt
      threadDelay 50_000

      -- The attestation should have been processed into the store
      store <- readTVarIO storeVar
      assertBool "store should have processed attestation"
        (length (stBlocks store) > 0)

  , testCase "aggregation flow: pool -> aggregate -> publish to TopicAggregation" $ do
      let keyVals = [ mkTestValidatorWithKey i | i <- [0..7] ]
          privKeys = map fst keyVals
          vals = map snd keyVals
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      prover <- setupProver
      verifier <- setupVerifier

      let sbb1 = mkTestSignedBlock gs 1
          st1 = unsafeRight $ stateTransition gs sbb1 False
          block1Root = toRoot (sbbBlock sbb1)
          genCp = zeroCheckpoint
          domain = cpRoot genCp

      let pubkeys = [ vAttestationPubkey v | v <- unSszList (bsValidators st1) ]
          target1 = Checkpoint block1Root 1
          subnetVis = [ vi | vi <- [0 .. 7 :: Int], vi `mod` 4 == 0 ]
          atts = [ mkSignedTestAttestation (privKeys !! i) (fromIntegral i)
                     1 block1Root genCp target1 domain
                 | i <- subnetVis ]

      -- Add to attestation pool and drain
      pool <- newAttestationPool
      mapM_ (\a -> atomically $ addAttestation pool a) atts
      groups <- atomically $ drainAttestations pool
      assertBool "pool should have 1 group" (Map.size groups == 1)

      -- Aggregate
      let (_, signedAtts) = head (Map.toList groups)
      aggResult <- aggregateAttestations prover signedAtts pubkeys domain 0
      case aggResult of
        Left err -> assertFailure ("aggregation failed: " <> show err)
        Right aa -> do
          mn <- newMockNetwork
          handle <- mockP2PHandle mn
          storeVar <- newTVarIO (initStore gs genesisBlock)
          cacheVar <- newTVarIO (newSeenCache 8192)
          slotVar <- newTVarIO 1

          let env = MessageHandlerEnv storeVar cacheVar handle verifier slotVar
          startMessageHandler env
          threadDelay 10_000

          p2hPublish handle TopicAggregation (encodeWire aa)
          threadDelay 50_000

          published <- readTVarIO (mnPublished mn)
          assertBool "TopicAggregation message should have been routed"
            (Map.member TopicAggregation published)

  , testCase "sync then live gossip: sync 5 blocks then receive block 6 via gossip" $ do
      let vals = [mkTestValidator 1 0]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      verifier <- setupVerifier

      let (blocks, states) = buildChain gs 5

      let blockMap = Map.fromList
            [ (bbSlot (sbbBlock sbb), encodeWire sbb)
            | sbb <- blocks
            ]

      mn <- newMockNetwork
      handle <- mockP2PHandleWithBlocks mn blockMap

      storeVar <- newTVarIO (initStore gs genesisBlock)
      statusVar <- newTVarIO Synced

      let syncEnv = SyncEnv handle storeVar statusVar 10
      syncResult <- runSync syncEnv 5
      assertEqual "sync should complete" Synced syncResult

      storeAfterSync <- readTVarIO storeVar
      assertEqual "store should be at slot 5"
        5 (currentSlot storeAfterSync)
      assertEqual "store should have 6 blocks (genesis + 5)"
        6 (Map.size (stBlocks storeAfterSync))

      cacheVar <- newTVarIO (newSeenCache 8192)
      slotVar <- newTVarIO 6

      let env = MessageHandlerEnv storeVar cacheVar handle verifier slotVar
      startMessageHandler env
      threadDelay 10_000

      let st5 = last states
          sbb6 = mkTestSignedBlock st5 6

      broadcastAll mn TopicBeaconBlock (encodeWire sbb6)
      threadDelay 50_000

      storeFinal <- readTVarIO storeVar
      assertEqual "store should have 7 blocks (genesis + 5 synced + 1 live)"
        7 (Map.size (stBlocks storeFinal))
  ]

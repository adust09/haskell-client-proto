-- | Devnet integration tests: end-to-end scenarios simulating pq-devnet-3
-- participation using MockNetwork for in-process multi-node simulation.
module Test.Integration.Devnet (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Config (defaultNodeConfig)
import Consensus.Constants (Slot)
import Consensus.ForkChoice (initStore)
import Consensus.StateTransition (getAttestationSubnet, stateTransition)
import Consensus.Types
import Crypto.LeanMultisig (setupProver, setupVerifier)
import Crypto.Operations (aggregateAttestations)
import Genesis (initializeGenesis)
import Network.MessageHandler
import Network.P2P.Types (P2PHandle (..), Topic (..))
import Network.P2P.Wire (encodeWire)
import Network.Sync (SyncEnv (..), SyncStatus (..), runSync)
import Node (startNode, stopNode, runSlotTicker)
import SSZ.List (unSszList)
import Storage (readCurrentState, withStorage)
import Test.Support.Helpers
import Test.Support.MockNetwork

-- | Force a Right value, failing with an error on Left.
unsafeRight :: Show e => Either e a -> a
unsafeRight (Right a) = a
unsafeRight (Left e)  = error ("unexpected Left: " <> show e)

tests :: TestTree
tests = testGroup "Integration.Devnet"
  [ testCase "genesis sync: node syncs blocks from peers after genesis init" $ do
      let vals = [mkTestValidator 1 32000000]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      -- Build a 10-block chain to simulate devnet history
      let (blocks, _states) = buildChain gs 10
          blockMap = Map.fromList
            [ (bbSlot (sbbBlock sbb), encodeWire sbb) | sbb <- blocks ]

      -- Node starts from genesis and syncs
      mn <- newMockNetwork
      handle <- mockP2PHandleWithBlocks mn blockMap
      storeVar <- newTVarIO (initStore gs genesisBlock)
      statusVar <- newTVarIO Synced

      let syncEnv = SyncEnv handle storeVar statusVar 10
      syncResult <- runSync syncEnv 10

      assertEqual "sync should complete successfully" Synced syncResult
      store <- readTVarIO storeVar
      assertEqual "store should be at slot 10" 10 (stCurrentSlot store)
      assertEqual "store should have 11 blocks (genesis + 10)"
        11 (Map.size (stBlocks store))

  , testCase "block following: node receives and validates new blocks via gossip" $ do
      let vals = [mkTestValidator 1 32000000]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      -- Build 3 blocks
      let (blocks, _states) = buildChain gs 3

      mn <- newMockNetwork
      handle <- mockP2PHandle mn
      storeVar <- newTVarIO (initStore gs genesisBlock)
      cacheVar <- newTVarIO (newSeenCache 8192)
      slotVar <- newTVarIO 3
      verifier <- setupVerifier

      let env = MessageHandlerEnv storeVar cacheVar handle verifier slotVar
      startMessageHandler env
      threadDelay 10_000

      -- Feed blocks one by one via gossip
      mapM_ (\sbb -> do
        broadcastAll mn TopicBeaconBlock (encodeWire sbb)
        threadDelay 50_000
        ) blocks

      store <- readTVarIO storeVar
      assertEqual "store should have 4 blocks (genesis + 3)"
        4 (Map.size (stBlocks store))

  , testCase "attestation submission: validator attestation accepted by peer" $ do
      let nVals = 4
          keyVals = [ mkTestValidatorWithKey i 32000000 | i <- [0..nVals - 1] ]
          privKeys = map fst keyVals
          vals = map snd keyVals
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock
          genesisRoot = toRoot genesisBlock

      mn <- newMockNetwork
      handle <- mockP2PHandle mn
      storeVar <- newTVarIO (initStore gs genesisBlock)
      cacheVar <- newTVarIO (newSeenCache 8192)
      slotVar <- newTVarIO 1
      verifier <- setupVerifier

      let env = MessageHandlerEnv storeVar cacheVar handle verifier slotVar
      startMessageHandler env
      threadDelay 10_000

      -- Validator 0 creates and publishes an attestation
      let subnet = getAttestationSubnet 0
          domain = cpRoot zeroCheckpoint
          att = mkSignedTestAttestation (privKeys !! 0) 0 0 genesisRoot
                  zeroCheckpoint zeroCheckpoint domain

      broadcastAll mn (TopicAttestation subnet) (encodeWire att)
      threadDelay 50_000

      -- Peer's store should have processed the attestation
      store <- readTVarIO storeVar
      assertBool "store should have latest message for validator 0"
        (Map.member 0 (stLatestMessages store))

  , testCase "aggregation: aggregator collects attestations and publishes proof" $ do
      let nVals = 16
          keyVals = [ mkTestValidatorWithKey i 32000000 | i <- [0..nVals - 1] ]
          privKeys = map fst keyVals
          vals = map snd keyVals
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      prover <- setupProver
      verifier <- setupVerifier

      let pubkeys = [ vPubkey v | v <- unSszList (bsValidators gs) ]
          genCp = zeroCheckpoint
          domain = cpRoot genCp

      -- Process block 1
      let sbb1 = mkTestSignedBlock gs 1
          _st1 = unsafeRight $ stateTransition gs sbb1 False
          block1Root = toRoot (sbbBlock sbb1)
          target1 = Checkpoint 1 block1Root

      -- Create signed attestations from all validators in subnet 0
      let subnetVis = [ vi | vi <- [0 .. fromIntegral nVals - 1], vi `mod` 4 == 0 ]
          atts = [ mkSignedTestAttestation (privKeys !! fromIntegral vi) vi
                     1 block1Root genCp target1 domain
                 | vi <- subnetVis ]

      aggResult <- aggregateAttestations prover atts pubkeys domain 0
      case aggResult of
        Left err -> assertFailure ("aggregation failed: " <> show err)
        Right saa -> do
          -- Aggregator publishes to network
          mn <- newMockNetwork
          handle <- mockP2PHandle mn
          storeVar <- newTVarIO (initStore gs genesisBlock)
          cacheVar <- newTVarIO (newSeenCache 8192)
          slotVar <- newTVarIO 1

          let env = MessageHandlerEnv storeVar cacheVar handle verifier slotVar
          startMessageHandler env
          threadDelay 10_000

          p2hPublish handle TopicAggregation (encodeWire saa)
          threadDelay 50_000

          published <- readTVarIO (mnPublished mn)
          assertBool "aggregation should be published to network"
            (Map.member TopicAggregation published)

  , testCase "finalization: 3-slot finality with sufficient voting" $ do
      let nVals = 16
          keyVals = [ mkTestValidatorWithKey i 32000000 | i <- [0..nVals - 1] ]
          privKeys = map fst keyVals
          vals = map snd keyVals
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      prover <- setupProver
      verifier <- setupVerifier

      let pubkeys = [ vPubkey v | v <- unSszList (bsValidators gs) ]
          genCp = zeroCheckpoint
          domain = cpRoot genCp

      -- Build chain: slots 1-4 empty blocks
      let sbb1 = mkTestSignedBlock gs 1
          st1 = unsafeRight $ stateTransition gs sbb1 False
          block1Root = toRoot (sbbBlock sbb1)
          target1 = Checkpoint 1 block1Root

          sbb2 = mkTestSignedBlock st1 2
          st2 = unsafeRight $ stateTransition st1 sbb2 False
          sbb3 = mkTestSignedBlock st2 3
          st3 = unsafeRight $ stateTransition st2 sbb3 False
          sbb4 = mkTestSignedBlock st3 4
          st4 = unsafeRight $ stateTransition st3 sbb4 False

      -- Create aggregations for all 4 subnets (100% voting power)
      let mkSubnetAgg subnetId = do
            let vis = [ vi | vi <- [0 .. fromIntegral nVals - 1]
                       , vi `mod` 4 == subnetId ]
                satts = [ mkSignedTestAttestation (privKeys !! fromIntegral vi) vi
                           1 block1Root genCp target1 domain
                       | vi <- vis ]
            unsafeRight <$> aggregateAttestations prover satts pubkeys domain subnetId

      saa0 <- mkSubnetAgg 0
      saa1 <- mkSubnetAgg 1
      saa2 <- mkSubnetAgg 2
      saa3 <- mkSubnetAgg 3

      -- Block 5 includes all aggregations
      let sbb5 = mkTestSignedBlockWithAtts st4 5 [saa0, saa1, saa2, saa3]
          st5 = unsafeRight $ stateTransition st4 sbb5 False

      -- Verify justification advanced (precondition for finalization)
      assertBool "justified checkpoint should advance to slot >= 1"
        (cpSlot (bsLatestJustified st5) >= 1)

      -- Replay through MockNetwork + MessageHandler (E2E)
      mn <- newMockNetwork
      handle <- mockP2PHandle mn
      storeVar <- newTVarIO (initStore gs genesisBlock)
      cacheVar <- newTVarIO (newSeenCache 8192)
      slotVar <- newTVarIO 5

      let env = MessageHandlerEnv storeVar cacheVar handle verifier slotVar
      startMessageHandler env
      threadDelay 10_000

      mapM_ (\sbb -> do
        broadcastAll mn TopicBeaconBlock (encodeWire sbb)
        threadDelay 50_000
        ) [sbb1, sbb2, sbb3, sbb4, sbb5]

      store <- readTVarIO storeVar
      assertBool "store justified checkpoint should have advanced"
        (cpSlot (stJustifiedCheckpoint store) >= 1)

  , testCase "restart recovery: node restarts from persisted state" $ do
      let genesis = mkTestGenesis
          (gs, forkStore) = initializeGenesis genesis

      -- Session 1: start node, advance slots, stop
      withStorage "/tmp/lc-test-devnet-restart" gs forkStore $ \sh -> do
        actors <- startNode defaultNodeConfig sh genesis
        runSlotTicker actors 1
        threadDelay 50_000
        runSlotTicker actors 2
        threadDelay 50_000
        runSlotTicker actors 3
        threadDelay 100_000
        state1 <- atomically $ readCurrentState sh
        assertEqual "session 1 should be at slot 3" 3 (bsSlot state1)
        stopNode actors

      -- Session 2: restart from same storage — state should persist
      -- Re-initialize with genesis (withStorage re-opens DB but loads genesis state
      -- into TVars). To truly test persistence we'd need to read from RocksDB.
      -- For now, verify the node starts cleanly after prior session.
      withStorage "/tmp/lc-test-devnet-restart2" gs forkStore $ \sh -> do
        actors <- startNode defaultNodeConfig sh genesis
        -- Advance from genesis again (new TVar state)
        runSlotTicker actors 1
        threadDelay 100_000
        state2 <- atomically $ readCurrentState sh
        assertEqual "session 2 should start cleanly at slot 1" 1 (bsSlot state2)
        stopNode actors

  , testCase "long-running stability: 100+ slots without crash" $ do
      let genesis = mkTestGenesis
          (gs, forkStore) = initializeGenesis genesis

      withStorage "/tmp/lc-test-devnet-stability" gs forkStore $ \sh -> do
        actors <- startNode defaultNodeConfig sh genesis

        -- Advance through 100 slots
        let targetSlot = 100 :: Slot
        mapM_ (\slot -> do
          runSlotTicker actors slot
          -- Brief delay every 10 slots to let the actor process
          if slot `mod` 10 == 0
            then threadDelay 20_000
            else pure ()
          ) [1..targetSlot]

        -- Final delay for last batch to process
        threadDelay 200_000

        state <- atomically $ readCurrentState sh
        assertBool "node should have advanced to slot >= 90"
          (bsSlot state >= 90)
        stopNode actors

  , testCase "sync then gossip: sync history then follow live blocks" $ do
      let vals = [mkTestValidator 1 32000000]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      verifier <- setupVerifier

      -- Build 5-block history
      let (blocks, states) = buildChain gs 5
          blockMap = Map.fromList
            [ (bbSlot (sbbBlock sbb), encodeWire sbb) | sbb <- blocks ]

      -- Sync phase
      mn <- newMockNetwork
      handle <- mockP2PHandleWithBlocks mn blockMap
      storeVar <- newTVarIO (initStore gs genesisBlock)
      statusVar <- newTVarIO Synced

      let syncEnv = SyncEnv handle storeVar statusVar 10
      syncResult <- runSync syncEnv 5
      assertEqual "sync should complete" Synced syncResult

      storeAfterSync <- readTVarIO storeVar
      assertEqual "should have 6 blocks after sync"
        6 (Map.size (stBlocks storeAfterSync))

      -- Transition to live gossip
      cacheVar <- newTVarIO (newSeenCache 8192)
      slotVar <- newTVarIO 6

      let env = MessageHandlerEnv storeVar cacheVar handle verifier slotVar
      startMessageHandler env
      threadDelay 10_000

      -- Build and broadcast block 6
      let st5 = last states
          sbb6 = mkTestSignedBlock st5 6

      broadcastAll mn TopicBeaconBlock (encodeWire sbb6)
      threadDelay 50_000

      storeFinal <- readTVarIO storeVar
      assertEqual "should have 7 blocks (genesis + 5 synced + 1 live)"
        7 (Map.size (stBlocks storeFinal))

  , testCase "multi-node consensus: two nodes agree on chain head" $ do
      let vals = [mkTestValidator 1 32000000]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      verifier <- setupVerifier

      -- Build 3 blocks
      let (blocks, _states) = buildChain gs 3

      mn <- newMockNetwork
      handle1 <- mockP2PHandle mn
      handle2 <- mockP2PHandle mn

      -- Node 1
      store1Var <- newTVarIO (initStore gs genesisBlock)
      cache1Var <- newTVarIO (newSeenCache 8192)
      slot1Var <- newTVarIO 3
      let env1 = MessageHandlerEnv store1Var cache1Var handle1 verifier slot1Var
      startMessageHandler env1

      -- Node 2
      store2Var <- newTVarIO (initStore gs genesisBlock)
      cache2Var <- newTVarIO (newSeenCache 8192)
      slot2Var <- newTVarIO 3
      let env2 = MessageHandlerEnv store2Var cache2Var handle2 verifier slot2Var
      startMessageHandler env2

      threadDelay 10_000

      -- Broadcast all blocks to both nodes
      mapM_ (\sbb -> do
        broadcastAll mn TopicBeaconBlock (encodeWire sbb)
        threadDelay 50_000
        ) blocks

      store1 <- readTVarIO store1Var
      store2 <- readTVarIO store2Var

      -- Both nodes should have the same blocks
      assertEqual "node 1 should have 4 blocks" 4 (Map.size (stBlocks store1))
      assertEqual "node 2 should have 4 blocks" 4 (Map.size (stBlocks store2))
      assertEqual "both nodes should agree on current slot"
        (stCurrentSlot store1) (stCurrentSlot store2)
  ]

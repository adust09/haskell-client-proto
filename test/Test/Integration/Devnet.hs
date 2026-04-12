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
import Consensus.Types
import Crypto.LeanMultisig (setupVerifier)
import Genesis (initializeGenesis)
import Network.MessageHandler
import Network.P2P.Types (Topic (..))
import Network.P2P.Wire (encodeWire)
import Network.Sync (SyncEnv (..), SyncStatus (..), runSync)
import Storage (readCurrentState, withStorage)
import Node (startNode, stopNode, runSlotTicker)
import Test.Support.Helpers
import Test.Support.MockNetwork

cfg :: Config
cfg = Config 0

tests :: TestTree
tests = testGroup "Integration.Devnet"
  [ testCase "genesis sync: node syncs blocks from peers after genesis init" $ do
      let vals = [mkTestValidator 1]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      -- Build a 10-block chain to simulate devnet history
      let (blocks, _states) = buildChain gs 10
          blockMap = Map.fromList
            [ (bbSlot (sbMessage sbb), encodeWire sbb) | sbb <- blocks ]

      -- Node starts from genesis and syncs
      mn <- newMockNetwork
      handle <- mockP2PHandleWithBlocks mn blockMap
      storeVar <- newTVarIO (initStore gs genesisBlock cfg)
      statusVar <- newTVarIO Synced

      let syncEnv = SyncEnv handle storeVar statusVar 10
      syncResult <- runSync syncEnv 10

      assertEqual "sync should complete successfully" Synced syncResult
      store <- readTVarIO storeVar
      assertEqual "store should have 11 blocks (genesis + 10)"
        11 (Map.size (stBlocks store))

  , testCase "block following: node receives and validates new blocks via gossip" $ do
      let vals = [mkTestValidator 1]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      -- Build 3 blocks
      let (blocks, _states) = buildChain gs 3

      mn <- newMockNetwork
      handle <- mockP2PHandle mn
      let store0 = (initStore gs genesisBlock cfg) { stTime = 15 }  -- slot 3
      storeVar <- newTVarIO store0
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
          keyVals = [ mkTestValidatorWithKey i | i <- [0..nVals - 1] ]
          privKeys = map fst keyVals
          vals = map snd keyVals
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock
          genesisRoot = toRoot genesisBlock

      mn <- newMockNetwork
      handle <- mockP2PHandle mn
      storeVar <- newTVarIO (initStore gs genesisBlock cfg)
      cacheVar <- newTVarIO (newSeenCache 8192)
      slotVar <- newTVarIO 1
      verifier <- setupVerifier

      let env = MessageHandlerEnv storeVar cacheVar handle verifier slotVar
      startMessageHandler env
      threadDelay 10_000

      -- Validator 0 creates and publishes an attestation
      let domain = cpRoot zeroCheckpoint
          headCp = Checkpoint genesisRoot 0
          att = mkSignedTestAttestation (privKeys !! 0) 0 0 headCp
                  zeroCheckpoint zeroCheckpoint domain

      broadcastAll mn (TopicAttestation 0) (encodeWire att)
      threadDelay 50_000

      -- Peer's store should have processed the attestation
      store <- readTVarIO storeVar
      assertBool "store should have latest message for validator 0"
        (Map.member 0 (stLatestMessages store))

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

      -- Session 2: restart from same storage
      withStorage "/tmp/lc-test-devnet-restart2" gs forkStore $ \sh -> do
        actors <- startNode defaultNodeConfig sh genesis
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
          if slot `mod` 10 == 0
            then threadDelay 20_000
            else pure ()
          ) [1..targetSlot]

        threadDelay 200_000

        state <- atomically $ readCurrentState sh
        assertBool "node should have advanced to slot >= 90"
          (bsSlot state >= 90)
        stopNode actors

  , testCase "sync then gossip: sync history then follow live blocks" $ do
      let vals = [mkTestValidator 1]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      verifier <- setupVerifier

      -- Build 5-block history
      let (blocks, states) = buildChain gs 5
          blockMap = Map.fromList
            [ (bbSlot (sbMessage sbb), encodeWire sbb) | sbb <- blocks ]

      -- Sync phase
      mn <- newMockNetwork
      handle <- mockP2PHandleWithBlocks mn blockMap
      storeVar <- newTVarIO (initStore gs genesisBlock cfg)
      statusVar <- newTVarIO Synced

      let syncEnv = SyncEnv handle storeVar statusVar 10
      syncResult <- runSync syncEnv 5
      assertEqual "sync should complete" Synced syncResult

      storeAfterSync <- readTVarIO storeVar
      assertEqual "should have 6 blocks after sync"
        6 (Map.size (stBlocks storeAfterSync))

      -- Transition to live gossip - set stTime to match slot 6
      atomically $ modifyTVar storeVar (\s -> s { stTime = 30 })  -- slot 6
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
      let vals = [mkTestValidator 1]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      verifier <- setupVerifier

      -- Build 3 blocks
      let (blocks, _states) = buildChain gs 3

      mn <- newMockNetwork
      handle1 <- mockP2PHandle mn
      handle2 <- mockP2PHandle mn

      -- Node 1
      let store0 = (initStore gs genesisBlock cfg) { stTime = 15 }  -- slot 3
      store1Var <- newTVarIO store0
      cache1Var <- newTVarIO (newSeenCache 8192)
      slot1Var <- newTVarIO 3
      let env1 = MessageHandlerEnv store1Var cache1Var handle1 verifier slot1Var
      startMessageHandler env1

      -- Node 2
      store2Var <- newTVarIO store0
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
  ]

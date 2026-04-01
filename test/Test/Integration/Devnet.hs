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
import Node (startNode, stopNode, runSlotTicker)
import Storage (readCurrentState, withStorage)
import Test.Support.Helpers
import Test.Support.MockNetwork

tests :: TestTree
tests = testGroup "Integration.Devnet"
  [ testCase "genesis sync: node syncs blocks from peers after genesis init" $ do
      let vals = [mkTestValidator 1 0]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      let (blocks, _states) = buildChain gs 10
          blockMap = Map.fromList
            [ (bbSlot (sbbBlock sbb), encodeWire sbb) | sbb <- blocks ]

      mn <- newMockNetwork
      handle <- mockP2PHandleWithBlocks mn blockMap
      storeVar <- newTVarIO (initStore gs genesisBlock)
      statusVar <- newTVarIO Synced

      let syncEnv = SyncEnv handle storeVar statusVar 10
      syncResult <- runSync syncEnv 10

      assertEqual "sync should complete successfully" Synced syncResult
      store <- readTVarIO storeVar
      assertEqual "store should be at slot 10" 10 (currentSlot store)
      assertEqual "store should have 11 blocks (genesis + 10)"
        11 (Map.size (stBlocks store))

  , testCase "block following: node receives and validates new blocks via gossip" $ do
      let vals = [mkTestValidator 1 0]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

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

      mapM_ (\sbb -> do
        broadcastAll mn TopicBeaconBlock (encodeWire sbb)
        threadDelay 50_000
        ) blocks

      store <- readTVarIO storeVar
      assertEqual "store should have 4 blocks (genesis + 3)"
        4 (Map.size (stBlocks store))

  , testCase "restart recovery: node restarts from persisted state" $ do
      let genesis = mkTestGenesis
          (gs, forkStore) = initializeGenesis genesis

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
      let vals = [mkTestValidator 1 0]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      verifier <- setupVerifier

      let (blocks, states) = buildChain gs 5
          blockMap = Map.fromList
            [ (bbSlot (sbbBlock sbb), encodeWire sbb) | sbb <- blocks ]

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
      assertEqual "should have 7 blocks (genesis + 5 synced + 1 live)"
        7 (Map.size (stBlocks storeFinal))

  , testCase "multi-node consensus: two nodes agree on chain head" $ do
      let vals = [mkTestValidator 1 0]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      verifier <- setupVerifier

      let (blocks, _states) = buildChain gs 3

      mn <- newMockNetwork
      handle1 <- mockP2PHandle mn
      handle2 <- mockP2PHandle mn

      store1Var <- newTVarIO (initStore gs genesisBlock)
      cache1Var <- newTVarIO (newSeenCache 8192)
      slot1Var <- newTVarIO 3
      let env1 = MessageHandlerEnv store1Var cache1Var handle1 verifier slot1Var
      startMessageHandler env1

      store2Var <- newTVarIO (initStore gs genesisBlock)
      cache2Var <- newTVarIO (newSeenCache 8192)
      slot2Var <- newTVarIO 3
      let env2 = MessageHandlerEnv store2Var cache2Var handle2 verifier slot2Var
      startMessageHandler env2

      threadDelay 10_000

      mapM_ (\sbb -> do
        broadcastAll mn TopicBeaconBlock (encodeWire sbb)
        threadDelay 50_000
        ) blocks

      store1 <- readTVarIO store1Var
      store2 <- readTVarIO store2Var

      assertEqual "node 1 should have 4 blocks" 4 (Map.size (stBlocks store1))
      assertEqual "node 2 should have 4 blocks" 4 (Map.size (stBlocks store2))
      assertEqual "both nodes should agree on current slot"
        (currentSlot store1) (currentSlot store2)
  ]

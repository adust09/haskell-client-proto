module Test.Network.Sync (tests) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Test.Tasty
import Test.Tasty.HUnit

import Consensus.ForkChoice (initStore, onBlock, onTick)
import Consensus.StateTransition (stateTransition)
import Consensus.Types
import Network.P2P.Wire (encodeWire)
import Network.Sync
import Test.Support.Helpers
import Test.Support.MockNetwork

cfg :: Config
cfg = Config 0

tests :: TestTree
tests = testGroup "Network.Sync"
  [ testCase "onBlock accepts blocks built by stateTransition" $ do
      let vals = [mkTestValidator 1 0]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock
          store0 = initStore gs genesisBlock cfg
          sbb1 = mkTestSignedBlock gs 1
          store1 = store0 { stTime = 5 }  -- slot 1 = time 5 / 5
      case onBlock store1 sbb1 of
        Left err -> assertFailure $ "onBlock failed: " <> show err
        Right store2 -> do
          Map.size (stBlocks store2) @?= 2

  , testCase "syncBatch applies one block" $ do
      let vals = [mkTestValidator 1 0]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock
          sbb1 = mkTestSignedBlock gs 1
          blockMap = Map.singleton (1 :: Word64) (encodeWire sbb1)

      mn <- newMockNetwork
      handle <- mockP2PHandleWithBlocks mn blockMap
      storeVar <- newTVarIO (initStore gs genesisBlock cfg)
      statusVar <- newTVarIO Synced
      let syncEnv = SyncEnv handle storeVar statusVar 64

      result <- syncBatch syncEnv 1 1
      case result of
        Left err -> assertFailure $ "syncBatch failed: " <> err
        Right slot -> slot @?= 1

  , testCase "sync from genesis to slot 3" $ do
      let vals = [mkTestValidator 1 0]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      -- Build a chain of 3 blocks
      let sbb1 = mkTestSignedBlock gs 1
      case stateTransition gs sbb1 False of
        Left err -> assertFailure $ "block 1 failed: " <> show err
        Right st1 -> do
          let sbb2 = mkTestSignedBlock st1 2
          case stateTransition st1 sbb2 False of
            Left err -> assertFailure $ "block 2 failed: " <> show err
            Right st2 -> do
              let sbb3 = mkTestSignedBlock st2 3
              case stateTransition st2 sbb3 False of
                Left err -> assertFailure $ "block 3 failed: " <> show err
                Right _ -> do
                  let blockMap = Map.fromList
                        [ (1, encodeWire sbb1)
                        , (2, encodeWire sbb2)
                        , (3, encodeWire sbb3)
                        ]
                  mn <- newMockNetwork
                  handle <- mockP2PHandleWithBlocks mn blockMap

                  storeVar <- newTVarIO (initStore gs genesisBlock cfg)
                  statusVar <- newTVarIO Synced
                  let syncEnv = SyncEnv handle storeVar statusVar 64

                  result <- runSync syncEnv 3
                  result @?= Synced

                  store <- readTVarIO storeVar
                  Map.size (stBlocks store) @?= 4  -- genesis + 3

  , testCase "sync with no blocks needed returns Synced" $ do
      let vals = [mkTestValidator 1 0]
          gs = mkTestGenesisState vals
          genesisBlock = mkTestGenesisBlock

      mn <- newMockNetwork
      handle <- mockP2PHandle mn
      storeVar <- newTVarIO (initStore gs genesisBlock cfg)
      statusVar <- newTVarIO Synced
      let syncEnv = SyncEnv handle storeVar statusVar 64

      result <- runSync syncEnv 0
      result @?= Synced
  ]

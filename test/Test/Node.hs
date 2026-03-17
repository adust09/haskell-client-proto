module Test.Node (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM

import Config (defaultNodeConfig)
import Genesis (initializeGenesis)
import Node (startNode, stopNode, runSlotTicker)
import Storage (readCurrentState, withStorage)
import Consensus.Types (BeaconState (..))
import Test.Support.Helpers (mkTestGenesis)

tests :: TestTree
tests = testGroup "Node"
  [ testCase "should start and stop cleanly" $ do
      let genesis = mkTestGenesis
          (gs, store) = initializeGenesis genesis
      withStorage "/tmp/lc-test-node-start" gs store $ \sh -> do
        actors <- startNode defaultNodeConfig sh genesis
        threadDelay 100000  -- 100ms to let actors spin up
        stopNode actors

  , testCase "should advance slot on BcSlotTick" $ do
      let genesis = mkTestGenesis
          (gs, store) = initializeGenesis genesis
      withStorage "/tmp/lc-test-node-tick" gs store $ \sh -> do
        actors <- startNode defaultNodeConfig sh genesis
        runSlotTicker actors 1
        threadDelay 100000  -- let actor process
        state <- atomically $ readCurrentState sh
        bsSlot state @?= 1
        stopNode actors

  , testCase "should advance multiple slots" $ do
      let genesis = mkTestGenesis
          (gs, store) = initializeGenesis genesis
      withStorage "/tmp/lc-test-node-multi" gs store $ \sh -> do
        actors <- startNode defaultNodeConfig sh genesis
        runSlotTicker actors 1
        threadDelay 50000
        runSlotTicker actors 2
        threadDelay 50000
        runSlotTicker actors 3
        threadDelay 100000
        state <- atomically $ readCurrentState sh
        bsSlot state @?= 3
        stopNode actors
  ]
